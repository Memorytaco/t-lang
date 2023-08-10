module Driver.Compiler.CodeGen.LLVM
  ( CodeGenT (..)
  , runCodeGen
  , LLVM (..)
  , emptyData
  , emptyState
  , runBasicBlock
  , runDefinition
  , runModule
  , runWithDefaultMain
  , withEntry
  , withEntryTop

  -- ** re-export
  , emptyIRBuilder
  , ShortByteString
  )
where


import Compiler.CodeGen.LLVM

import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Constant as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM (typeOf)
import qualified LLVM.IRBuilder.Constant as LLVMC
import LLVM.AST.Operand (Operand)

import qualified LLVM.IRBuilder.Monad as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import LLVM.IRBuilder.Module (ModuleBuilderT (..), MonadModuleBuilder, runModuleBuilderT, ModuleBuilderState, buildModuleT)
import LLVM.IRBuilder.Monad (IRBuilderT (..), runIRBuilderT, MonadIRBuilder, IRBuilderState, emptyIRBuilder)

import Capability.Accessors
import Capability.Reader (HasReader, MonadReader (..), local)
import Capability.State (HasState, MonadState (..))
import Capability.Sink (HasSink)
import Capability.Source (HasSource)

import Data.ByteString.Short (ShortByteString)
import Data.String (IsString)

import Control.Monad.Trans (lift)
import Control.Monad (void)
import Control.Monad.State (StateT (..))
import Control.Monad.Reader (ReaderT (..))
import GHC.Generics (Generic)
import Compiler.Backend.LLVM.Runtime (wrapMain)

data CodeGenData m name = CodeGenData
  { localval :: [(name, (LLVM.Type, Operand))]
  , destruct :: [(LLVM.Type, Operand)]
  , context :: m (LLVM.Type, Operand)
  , isPattern :: Bool
  } deriving (Generic)

data CodeGenState name = CodeGenState
  { resource :: GlobalResource
  , unnamed :: Word
  , globalval :: [(name, (LLVM.Type, Operand))]
  , bindings :: [(name, (LLVM.Type, Operand))]
  } deriving Generic

-- | this should always be the base monad whenever code generation for llvm happening and
-- it is default a monad transformer since plain monad is used rarely in the codebase
newtype LLVM m a = LLVM
  { runLLVM :: IRBuilderT (ModuleBuilderT m) a
  } deriving newtype (Functor, Applicative, Monad, MonadFail)
    deriving newtype (MonadModuleBuilder, MonadIRBuilder)

-- | a helper to define capability
--
-- * `t` is what occurs recursive
-- * `m` and `name` are parameters for `t`
type T t m name = StateT (CodeGenState name) (ReaderT (CodeGenData (t m name) name) (LLVM m))

-- | a customised environment to let every thing about code generation happen
newtype CodeGenT m name a = CodeGenT
  { runCodeGenT :: T CodeGenT m name a
  } deriving newtype (Functor, Applicative, Monad, MonadFail)
    deriving newtype (MonadModuleBuilder, MonadIRBuilder)
    -- reader definition
    deriving (HasReader "local" [(name, (LLVM.Type, Operand))], HasSource "local" [(name, (LLVM.Type, Operand))])
        via Rename "localval" (Field "localval" () (MonadReader (T CodeGenT m name)))
    deriving (HasReader "destruct" [(LLVM.Type, Operand)], HasSource "destruct" [(LLVM.Type, Operand)])
        via (Field "destruct" () (MonadReader (T CodeGenT m name)))
    deriving ( HasReader "context" (CodeGenT m name (LLVM.Type, Operand))
             , HasSource "context" (CodeGenT m name (LLVM.Type, Operand)))
        via (Field "context" () (MonadReader (T CodeGenT m name)))
    deriving (HasReader "isPattern" Bool, HasSource "isPattern" Bool)
        via (Field "isPattern" () (MonadReader (T CodeGenT m name)))

    -- state definition
    deriving (HasState "resource" GlobalResource, HasSink "resource" GlobalResource, HasSource "resource" GlobalResource)
        via Field "resource" () (MonadState (T CodeGenT m name))
    deriving (HasState "unnamed" Word, HasSink "unnamed" Word, HasSource "unnamed" Word)
        via Field "unnamed" () (MonadState (T CodeGenT m name))
    deriving ( HasState "global" [(name, (LLVM.Type, Operand))]
             , HasSource "global" [(name, (LLVM.Type, Operand))]
             , HasSink "global" [(name, (LLVM.Type, Operand))])
        via Rename "globalval" (Field "globalval" () (MonadState (T CodeGenT m name)))
    deriving ( HasState "pattern" [(name, (LLVM.Type, Operand))]
             , HasSource "pattern" [(name, (LLVM.Type, Operand))]
             , HasSink "pattern" [(name, (LLVM.Type, Operand))])
        via Rename "bindings" (Field "bindings" () (MonadState (T CodeGenT m name)))

emptyData :: Monad m => CodeGenData (CodeGenT m name) name
emptyData = CodeGenData [] [] (return (LLVM.i1, LLVM.ConstantOperand $ LLVM.Int 1 1)) False
emptyState :: CodeGenState name
emptyState = CodeGenState (GlobalResource mempty) 0 [] []

runCodeGen :: CodeGenState name -> CodeGenData (CodeGenT m name) name -> CodeGenT m name a
           -> LLVM m (a, CodeGenState name)
runCodeGen stat dat m = runReaderT (runStateT (runCodeGenT m) stat) dat

runBasicBlock :: Monad m
              => IRBuilderState -> LLVM m a -> ModuleBuilderT m (a, [LLVM.BasicBlock])
runBasicBlock stat m = runIRBuilderT stat (runLLVM m)

runDefinition :: Monad m
              => (IRBuilderState, ModuleBuilderState)
              -> LLVM m a -> m ((a, [LLVM.BasicBlock]), [LLVM.Definition])
runDefinition (b, a) m = runModuleBuilderT a (runBasicBlock b m)

runModule :: Monad m => ShortByteString -> IRBuilderState -> LLVM m a -> m LLVM.Module
runModule name stat m = buildModuleT name (runBasicBlock stat m)

liftLLVM :: Monad m => LLVM m a -> CodeGenT m name a
liftLLVM = CodeGenT . lift . lift

runWithDefaultMain :: (IsString name, Monad m) =>
  ShortByteString -> CodeGenT m name Operand -> m LLVM.Module
runWithDefaultMain name m =
  runModule name emptyIRBuilder . runCodeGen emptyState emptyData $
  wrapMain \names -> local @"local" (<> names) m

withEntry :: (IsString name, Monad m) => CodeGenT m name Operand -> LLVM m Operand
withEntry m = do
  LLVM . IRBuilderT . lift
    $ LLVM.function "main" [(LLVM.i32, LLVM.ParameterName "argc"), (LLVM.ptr, LLVM.ParameterName "argv")] LLVM.i32
    \case
    [argc, argv] -> do
      let start = LLVM.fresh `LLVM.named` "start" >>= LLVM.emitBlockStart
          end a = do
            v <- LLVM.typeOf a
            case v of
              Right (LLVM.IntegerType _) -> LLVM.ret a
              _ -> LLVM.ret (LLVMC.int32 0)
          preData = emptyData { localval = [("argc", (LLVM.i32, argc)), ("argv", (LLVM.ptr, argv))]}
      void $ runLLVM (runCodeGen emptyState preData (start >> m >>= end))
    _ -> error "impossible in Driver.CodeGen"

-- withEntry :: (IsString name, Monad m) => CodeGenT m name Operand -> LLVM m Operand
-- withEntry m = do
--   LLVM . IRBuilderT . lift
--     $ LLVM.function "main" [(LLVM.i32, LLVM.ParameterName "argc"), (LLVM.ptr, LLVM.ParameterName "argv")] LLVM.i32
--     \case
--     [argc, argv] -> do
--       let start = LLVM.fresh `LLVM.named` "start" >>= LLVM.emitBlockStart
--           end a = do
--             v <- LLVM.typeOf a
--             case v of
--               Right (LLVM.IntegerType _) -> LLVM.ret a
--               _ -> LLVM.ret (LLVMC.int32 0)
--           preData = emptyData { localval = [("argc", (LLVM.i32, argc)), ("argv", (LLVM.ptr, argv))]}
--       void $ runLLVM (runCodeGen emptyState preData (start >> m >>= end))
--     _ -> error "impossible in Driver.CodeGen"

withEntryTop :: (Monad m, IsString name1) => CodeGenT m name1 Operand -> CodeGenT m name2 Operand
withEntryTop m = liftLLVM (withEntry m)
