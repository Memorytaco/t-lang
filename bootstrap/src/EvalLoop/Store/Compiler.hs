module EvalLoop.Store.Compiler
  (

    EvalCompilerStore (..)
  , stageStore
  , operators
  , thisModule
  , compiledModules
  , evalJITSession

  , newEvalCompilerStore

  , jitContext
  , jitSession
  , jitSessions
  , module Compiler.Store
  )
where

import Language.Core (Name, OperatorStore, builtinStore, ModuleSurface, Module (..), ModuleName (..), DeclStore (..))
import Compiler.Store

import JIT.LLVM as JIT

import qualified LLVM.Module as LLVM (Module)

import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)

data EvalCompilerStore
  = EvalCompilerStore
    { _stageStore :: StageStore
    , _operators  :: OperatorStore
    -- | hold declarations for current repl
    , _thisModule :: ModuleSurface
    , _compiledModules :: [(Name, LLVM.Module)]
    , _evalJITSession :: EvalJITSession
    } deriving Generic

data EvalJITSession
  = EvalJITSession
    { _jitContext :: JIT.LLVMJITContext -- ^ main context, to which ShellJITSession's lifetime is bound
    , _jitSession :: JIT.JITClass -- ^ current class, in which symbols are linked and executed
    , _jitSessions :: [String]  -- ^ available JITClasses, with a unique name for each
    } deriving Generic

makeLenses ''EvalJITSession
makeLenses ''EvalCompilerStore

newEvalCompilerStore :: MonadIO m => Name -> String -> m EvalCompilerStore
newEvalCompilerStore name sessionName =
  newDefaulEvalJITSession sessionName <&> EvalCompilerStore initStageStore builtinStore (Module (ModuleName [] name) [] (DeclStore [])) []

newDefaulEvalJITSession :: MonadIO m => String -> m EvalJITSession
newDefaulEvalJITSession name = do
  (ctx, session) <- JIT.createLLVMJITContextDefault >>= JIT.newJITClass name
  return $ EvalJITSession ctx session [name]
