{- * Module where we emit llvm ir code

  Resolve type -> generate LLVM IR -> return the IR Module
-}

module Compiler.Backend.LLVM
  (
    OperandGen (..)
  , genExpr

  , GlobalResource (..)
  )
where

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import qualified LLVM.IRBuilder.Constant as LLVMC
import LLVM.AST.Operand (Operand)
import LLVM.IRBuilder

import Language.Core hiding (Type, Constraint)
import Tlang.Generic ((:+:) (..))
import Language.Core.Extension

import Data.Text (Text)
import Data.Kind (Constraint, Type)
import Data.Functor.Foldable (cata)
import Capability.Reader (HasReader, asks, ask, local)
import Capability.State (HasState, gets, modify, get)
import Control.Monad (when, foldM, forM, join)
import Data.Text (unpack)
import qualified Data.Map as Map

class OperandGen m f where
  type CodeGenEnv (m :: Type -> Type) (f :: Type -> Type) :: Constraint
  genOperand :: (MonadModuleBuilder m, MonadIRBuilder m, CodeGenEnv m f)
             => f (m (LLVM.Type, Operand)) -> m (LLVM.Type, Operand)

genExpr :: ( HasReader "local" [(name, (LLVM.Type, Operand))] m
           , HasState "global" [(name, (LLVM.Type, Operand))] m
           , MonadFail m
           , Show name, Eq name
           , MonadModuleBuilder m, MonadIRBuilder m
           , OperandGen m f, CodeGenEnv m f, Functor f
           )
        => Expr f name -> m (LLVM.Type, Operand)
genExpr = cata go
  where
    go (ValF name) = gets @"global" (lookup name) >>= \case
      Just a -> return a
      Nothing -> asks @"local" (lookup name) >>= \case
        Just a -> return a
        Nothing -> fail $ "No definition for name " <> show name
    go (ExprF fv) = genOperand fv

instance (OperandGen m f, OperandGen m g) => OperandGen m (f :+: g) where
  type CodeGenEnv m (f :+: g) = (CodeGenEnv m f, CodeGenEnv m g)
  genOperand (Inl fv) = genOperand fv
  genOperand (Inr fv) = genOperand fv

instance OperandGen m (Value typ) where
  type CodeGenEnv m (Value typ) = (MonadFail m)
  genOperand _ = fail "VisibleType is not supported for now"

instance OperandGen m Apply where
  type CodeGenEnv m Apply = (MonadFail m)
  genOperand (Apply mf ma mas) = do
    (ftyp, f) <- mf
    (typ, args) <- case ftyp of
      LLVM.FunctionType res args _ -> return (res, args)
      _ -> fail "Invalid call site, expect a function"
    when (length args /= (length mas + 1)) $
       fail $ "function expect " <> show (length args) <> " arguments, but supplied with " <> show (length mas + 1)
    (ta, a) <- ma
    (tas, as) <- unzip <$> sequence mas
    when (ta:tas /= args) $ fail "Mismatched argument type"
    res <- call typ f ((,[]) <$> a:as)
    return (typ, res)

data GlobalResource = GlobalResource
  { strStore :: Map.Map String Operand
  }

instance OperandGen m LiteralText where
  type CodeGenEnv m LiteralText = (HasState "resource" GlobalResource m, HasState "unnamed" Word m)
  genOperand (LiteralText (Literal (unpack -> text))) = do
    resource <- gets @"resource" strStore
    case Map.lookup text resource of
      Just val -> return (LLVM.ptr, val)
      Nothing -> do
        name <- modify @"unnamed" (+1) >> get @"unnamed"
        val <- LLVM.ConstantOperand <$> globalStringPtr text (LLVM.UnName name)
        modify @"resource" \r -> r { strStore = Map.insert text val (strStore r) }
        return (LLVM.ptr, val)

instance OperandGen m LiteralNumber where
  type CodeGenEnv m LiteralNumber = ()
  genOperand (LiteralNumber (Literal num)) = return (LLVM.double , LLVMC.double num)

instance OperandGen m LiteralInteger where
  type CodeGenEnv m LiteralInteger = ()
  genOperand (LiteralInteger (Literal num)) = return (LLVM.i32 , LLVMC.int32 num)

instance OperandGen m Tuple where
  type CodeGenEnv m Tuple = ()
  genOperand (Tuple ms) = do
    vals <- sequence ms
    let typ = LLVM.StructureType False $ fst <$> vals
    ptr <- alloca typ Nothing 1
    empty <- load typ ptr 1
    final <- foldM (\b (val, ix) -> insertValue b val [ix]) empty (zip (snd <$> vals) [0..])
    store ptr 1 final
    (typ,) <$> load typ ptr 1

instance OperandGen m binder => OperandGen m (Let binder) where
  type CodeGenEnv m (Let binder) =
    ( CodeGenEnv m binder, HasState "unnamed" Word m
    , HasReader "destruct" [(LLVM.Type, Operand)] m
    , HasReader "context" (m (LLVM.Type, Operand)) m
    )
  genOperand (Let binder ma mv) = ma >>= \a ->
    local @"destruct" (const [a]) . local @"context" (const mv)
    $ genOperand binder

instance OperandGen m (Grp g) where
  type CodeGenEnv m (Grp g) = ()
  genOperand _ = error "not implemented"

-- | a pattern itself is no more than a parser combinator targeting
-- binary value and equipped with some effects
--
-- the generated code for a pattern should return whether this is a
-- successful matching and outer most pattern should take care
-- of value returned by sub patterns, then it should apply
-- accumulated effects to its branch value.
--
instance OperandGen m lit => OperandGen m (Pattern lit ext label name) where
  type CodeGenEnv m (Pattern lit ext label name) =
    ( HasState "unnamed" Word m
    , HasReader "destruct" [(LLVM.Type, Operand)] m
    , HasReader "isPattern" Bool m
    , HasReader "context" (m (LLVM.Type, Operand)) m
    , HasReader "local" [(name, (LLVM.Type, Operand))] m
    , HasState "pattern" [(name, (LLVM.Type, Operand))] m
    , MonadFail m, Functor lit, Functor ext
    , CodeGenEnv m lit
    )
  genOperand v = do
    (binds, _) <- cata go v
    ask @"context" >>= local @"local" (binds <>)
    where
      ret a = (LLVM.i1, LLVMC.bit a)
      getValue = ask @"destruct" >>= \case
        a:_ -> return a
        _ -> fail "Internal error: pattern var doesn't have destructor value"
      go PatWildF = return ([], ret 1)
      go PatUnitF = return ([], ret 0)  -- FIXME: complete definition
      go (PatVarF name) = getValue >>= \a -> return ([(name, a)], ret 1)
      go (PatPrmF _) = error "undefined literal match" -- local @"isPattern" (const True) $ genOperand lv
      go (PatTupF ls) = do
        (typ, val) <- getValue
        typs <- case typ of
          LLVM.StructureType _ typs ->
            if length typs == length ls
               then return typs
               else fail "mismatched runtime type for tuple"
          _ -> fail "mismatched runtime type for tuple"
        let operands = extractValue val . pure <$> [0.. (fromInteger . toInteger $ length ls - 1)]
        res <- forM (zip operands ls) \(mval, mend) -> do
          v <- mval
          t <- LLVM.typeOf v >>= \case
            Right t -> return t
            Left err -> fail $ "LLVM: " <> err
          local @"destruct" (const [(t, v)]) mend
        return (join $ fst <$> res, ret 1)
      go _ = error "not yet defined pattern code"

instance OperandGen m (Record Label) where
  type CodeGenEnv m (Record Label) = ()
  genOperand _ = do
    error "record not defined"

instance OperandGen m (Selector l) where
  type CodeGenEnv m (Selector l) = ()
  genOperand _ = do
    error "selector not defined"

instance OperandGen m (Constructor l) where
  type CodeGenEnv m (Constructor l) = ()
  genOperand _ = do
    error "constructor not defined"

instance OperandGen m pattern' => OperandGen m (Equation pattern' prefix) where
  type CodeGenEnv m (Equation pattern' prefix) = ()
  genOperand _ = do
    error "lambda not defined"

instance OperandGen m ((@:) typ) where
  type CodeGenEnv m ((@:) typ) = ()
  genOperand (v :@ _) = v

-- **** global definition

type GlobalGenEnv :: (Type -> Type) -> (Type -> Type) -> Type -> Constraint
type family GlobalGenEnv m f info
class GlobalGen f info where
  genGlobal :: (GlobalGenEnv m f info, MonadModuleBuilder m, MonadIRBuilder m)
            => f info -> m (Maybe Operand)

declGen :: (MonadModuleBuilder m, MonadIRBuilder m, GlobalGen decls name, GlobalGenEnv m decls name)
        => Decl decls name -> m (Maybe Operand)
declGen (Decl decl) = genGlobal decl

type instance GlobalGenEnv m (f :+: g) a = (GlobalGenEnv m f a, GlobalGenEnv m g a)
instance (GlobalGen f a, GlobalGen g a) => GlobalGen (f :+: g) a where
  genGlobal (Inl fv) = genGlobal fv
  genGlobal (Inr fv) = genGlobal fv

-- type instance GlobalGenEnv m (UserType typ [Prefix Name typ]) a = ()
-- instance GlobalGen (UserType typ [Prefix Name typ]) a where
--   genGlobal _ = return Nothing

type instance GlobalGenEnv m (Item (UserOperator Text)) a = ()
instance GlobalGen (Item (UserOperator Text)) a where
  genGlobal _ = return Nothing

type instance GlobalGenEnv m (Item (FFI typ Name)) a = ()
instance GlobalGen (Item (FFI typ Name)) Name where
  genGlobal (Item _ name) = return Nothing

type instance GlobalGenEnv m (UserValue expr (Maybe typ)) a = ()
instance GlobalGen (UserValue expr (Maybe typ)) a where
  genGlobal _ = return Nothing

--   :+: UserData [Prefix Name typ] (UserDataDef (UserPhantom :+: UserCoerce :+: UserEnum Label :+: UserStruct Label) typ)
