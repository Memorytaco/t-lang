{- * Module where we generate llvm IR
--
-- 
-}

module Compiler.CodeGen.LLVM
  (
    LLVMIRGen (..)
  , genExpr
  , declGen
  )
where

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import qualified LLVM.IRBuilder.Constant as LLVMC
import LLVM.AST.Operand (Operand)
import LLVM.IRBuilder

import Language.Core hiding (Type, Constraint)
import Language.Generic ((:+:) (..))
import Language.Core.Extension

import Compiler.Backend.LLVM.Definition (MonadLLVMBuilder)
import Compiler.Backend.LLVM.Runtime (globalString)

import Data.Text (Text, unpack)
import Data.Kind (Constraint, Type)
import Data.Functor.Foldable (cata)
import Capability.Reader (HasReader, asks, ask, local)
import Capability.State (HasState, gets, modify, get)
import Control.Monad (when, foldM, forM)

class LLVMIRGen m f where
  type LLVMIRGenContext (m :: Type -> Type) (f :: Type -> Type) :: Constraint
  genOperand :: (MonadLLVMBuilder m, LLVMIRGenContext m f)
             => f (m (LLVM.Type, Operand)) -> m (LLVM.Type, Operand)

genExpr :: ( HasReader "local" [(name, (LLVM.Type, Operand))] m
           , HasState "global" [(name, (LLVM.Type, Operand))] m
           , MonadFail m
           , Show name, Eq name
           , MonadModuleBuilder m, MonadIRBuilder m
           , LLVMIRGen m f, LLVMIRGenContext m f, Functor f
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

instance (LLVMIRGen m f, LLVMIRGen m g) => LLVMIRGen m (f :+: g) where
  type LLVMIRGenContext m (f :+: g) = (LLVMIRGenContext m f, LLVMIRGenContext m g)
  genOperand (Inl fv) = genOperand fv
  genOperand (Inr fv) = genOperand fv

instance LLVMIRGen m (Value typ) where
  type LLVMIRGenContext m (Value typ) = (MonadFail m)
  genOperand _ = fail "VisibleType is not supported for now"

instance LLVMIRGen m Apply where
  type LLVMIRGenContext m Apply = (MonadFail m)
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

instance LLVMIRGen m LiteralText where
  type LLVMIRGenContext m LiteralText = (HasState "unnamed" Word m)
  genOperand (LiteralText (Literal (unpack -> text))) = do
    name <- modify @"unnamed" (+1) >> get @"unnamed"
    val <- globalString text (LLVM.UnName name)
    return (LLVM.ptr, val)

instance LLVMIRGen m LiteralNumber where
  type LLVMIRGenContext m LiteralNumber = ()
  genOperand (LiteralNumber (Literal num)) = return (LLVM.double , LLVMC.double num)

instance LLVMIRGen m LiteralInteger where
  type LLVMIRGenContext m LiteralInteger = ()
  genOperand (LiteralInteger (Literal num)) = return (LLVM.i32 , LLVMC.int32 num)

instance LLVMIRGen m Tuple where
  type LLVMIRGenContext m Tuple = ()
  genOperand (Tuple ms) = do
    vals <- sequence ms
    let typ = LLVM.StructureType False $ fst <$> vals
    ptr <- alloca typ Nothing 1
    empty <- load typ ptr 1
    final <- foldM (\b (val, ix) -> insertValue b val [ix]) empty (zip (snd <$> vals) [0..])
    store ptr 1 final
    (typ,) <$> load typ ptr 1

instance LLVMIRGen m binder => LLVMIRGen m (Let binder) where
  type LLVMIRGenContext m (Let binder) =
    ( LLVMIRGenContext m binder, HasState "unnamed" Word m
    , HasReader "destruct" [(LLVM.Type, Operand)] m
    , HasReader "context" (m (LLVM.Type, Operand)) m
    )
  genOperand (Let binder ma mv) = ma >>= \a ->
    local @"destruct" (const [a]) . local @"context" (const mv)
    $ genOperand binder

instance LLVMIRGen m (Grp g) where
  type LLVMIRGenContext m (Grp g) = ()
  genOperand _ = error "not implemented"

-- | a pattern itself is no more complex than a parser combinator targeting
-- binary value and equipped with some effects.
--
-- the generated code for a pattern should return whether this is a
-- successful matching and outer most pattern should take care
-- of value returned by sub patterns, then it should apply
-- accumulated effects to its branch value.
--
instance LLVMIRGen m lit => LLVMIRGen m (Pattern lit ext label name) where
  type LLVMIRGenContext m (Pattern lit ext label name) =
    ( HasState "unnamed" Word m
    , HasReader "destruct" [(LLVM.Type, Operand)] m
    , HasReader "isPattern" Bool m
    , HasReader "context" (m (LLVM.Type, Operand)) m
    , HasReader "local" [(name, (LLVM.Type, Operand))] m
    , HasState "pattern" [(name, (LLVM.Type, Operand))] m
    , MonadFail m, Functor lit, Functor ext
    , LLVMIRGenContext m lit
    )
  genOperand v = do
    (binds, _) <- cata go v
    ask @"context" >>= local @"local" (binds <>)
    where
      retBool a = (LLVM.i1, LLVMC.bit a)
      getValue = ask @"destruct" >>= \case
        a:_ -> return a
        _ -> fail "Internal error: pattern var doesn't have destructor value"
      go PatWildF = return ([], retBool 1)
      go PatUnitF = return ([], retBool 0)  -- FIXME: complete definition
      go (PatVarF name) = getValue >>= \a -> return ([(name, a)], retBool 1)
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
        return (fst =<< res, retBool 1)
      go _ = error "not yet defined pattern code"

instance LLVMIRGen m (Record Label) where
  type LLVMIRGenContext m (Record Label) = ()
  genOperand _ = do
    error "record not defined"

instance LLVMIRGen m (Selector l) where
  type LLVMIRGenContext m (Selector l) = ()
  genOperand _ = do
    error "selector not defined"

instance LLVMIRGen m (Constructor l) where
  type LLVMIRGenContext m (Constructor l) = ()
  genOperand _ = do
    error "constructor not defined"

instance LLVMIRGen m pattern' => LLVMIRGen m (Equation pattern' prefix) where
  type LLVMIRGenContext m (Equation pattern' prefix) = ()
  genOperand _ = do
    error "lambda not defined"

instance LLVMIRGen m ((:::) typ) where
  type LLVMIRGenContext m ((:::) typ) = ()
  genOperand (_ ::: v) = v

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
