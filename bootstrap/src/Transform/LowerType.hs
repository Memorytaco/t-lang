{-# LANGUAGE RankNTypes #-}
{- | This module is used to transform things into low level type (runtime type)

    TODO: This module should have similar structure to `Transform.TypeGraph` module
-}
module Transform.LowerType
  (

    LowerType (..)

  , lowerTuple
  , lowerRecord

  , lowerTypeRep
  , lowerTypeRepPartial
  )
where

-- import Data.Kind (Type, Constraint)

import qualified Language.Core as AST (Type (..))
import Language.Core hiding (Type, Constraint)
import Language.Core.Extension

import Tlang.Rep
import Language.Generic
-- import Tlang.Constraint (Prefix (..))

-- import Capability.Reader (HasReader)
import Control.Lens ( (<&>), (^.) )
import Control.Monad (forM)
import Data.List (sortBy)
import Data.Kind (Type, Constraint)

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.AddrSpace as LLVM

-- | class for folding foldable into lower types
class LowerType (f :: Type -> Type) (typ :: Type) where
  type LowerTypeContext f typ (m :: Type -> Type) :: Constraint
  lowerType :: LowerTypeContext f typ m => f (m typ) -> m typ


----------------------------------------
-- ** lower anything into representation
----------------------------------------

instance LowerType Tuple (Rep a) where
  type LowerTypeContext Tuple (Rep a) m = (Monad m)
  lowerType (Tuple ms) = forM ms (fmap $ Embed . (^. _Rep)) <&> Rep . RepLift . struct False

-- | record type fields are sorted by its labels
instance Ord label => LowerType (Record label) (Rep a) where
  type LowerTypeContext (Record label) (Rep a) m = (Monad m)
  lowerType (Record ms) = forM (snd <$> sortBy (\(fst -> a) (fst -> b) -> compare a b) ms) (fmap $ Embed . (^. _Rep))
                      <&> Rep . RepLift . struct False

-- | sequential type
instance LowerType [] (Rep a) where
  type LowerTypeContext [] (Rep a) m = Monad m
  lowerType ms = forM ms (fmap $ Embed . (^. _Rep)) <&> Rep . RepLift . struct False

instance LowerType (Variant label) (Rep a) where
  type LowerTypeContext (Variant label) (Rep a) m = Monad m
  lowerType (Variant _ms) = error "Lowering variant types is not implemented"

instance (Functor rep, Functor (bind name), LowerType (bind name) (Rep a), LowerType rep (Rep a)) => LowerType (AST.Type bind rep name) (Rep a) where
  type LowerTypeContext (AST.Type bind rep name) (Rep a) m = (LowerTypeContext rep (Rep a) m, LowerTypeContext (bind name) (Rep a) m, MonadFail m)
  lowerType = cata go
    where
      go TypPhtF = fail "Invalid Bottom ecountered during lowering type"
      go (TypVarF ma) = ma
      go (TypConF _ _) = fail "Invalid Type Application ecountered during lowering type"
      go (TypBndF ma) = lowerType ma
      go (TypeF rma) = lowerType rma

instance (LowerType f (Rep a), LowerType g (Rep a)) => LowerType (f :+: g) (Rep a) where
  type LowerTypeContext (f :+: g) (Rep a) m = (LowerTypeContext f (Rep a) m, LowerTypeContext g (Rep a) m)
  lowerType (Inl v) = lowerType v
  lowerType (Inr v) = lowerType v

----------------------------
-- ** lower Rep into LLVM ir
----------------------------

instance LowerType Rep LLVM.Type where
  type LowerTypeContext Rep LLVM.Type m = Monad m
  lowerType (Rep a) = lowerType a

instance (LowerType seq LLVM.Type, Functor seq) => LowerType (PrimitiveT seq) LLVM.Type where
  type LowerTypeContext (PrimitiveT seq) LLVM.Type m = (Monad m, LowerTypeContext seq LLVM.Type m)
  lowerType = cata \case
    VoidTF -> return LLVM.VoidType
    PtrF _ Nothing -> return $ LLVM.PointerType (LLVM.AddrSpace 0)
    PtrF _ (Just addr) -> return . LLVM.PointerType . LLVM.AddrSpace $ fromInteger addr
    ScalaF typ -> return $ encodeLLVMType typ
    StructF ms isPacked -> sequence ms <&> LLVM.StructureType isPacked
    SeqF fm -> lowerType fm
    EmbedF m -> m

instance (Functor t, LowerType t LLVM.Type) => LowerType (DataRep t) LLVM.Type where
  type LowerTypeContext (DataRep t) LLVM.Type m = (Monad m, LowerTypeContext t LLVM.Type m)
  lowerType = cata \case
    DataRepF m -> m
    RepLiftF fma -> lowerType fma

isAggregrateLLVMType :: LLVM.Type -> Bool
isAggregrateLLVMType = \case
  LLVM.StructureType _ _ -> True
  LLVM.ArrayType _ _ -> True
  _ -> False

instance LowerType SeqT LLVM.Type where
  type LowerTypeContext SeqT LLVM.Type m = Monad m
  lowerType (SeqVector m len) = m <&> \t ->
    if isAggregrateLLVMType t
    then LLVM.ArrayType (fromInteger len) t
    else LLVM.VectorType (fromInteger len) t
  lowerType (SeqArray m len'maybe) = m <&> \t ->
    let len = maybe 0 fromInteger len'maybe in LLVM.ArrayType len t


-----------------------------------------------------------------------------------------
-- | lowering partial type, reduce some highlevel constructors into primitive constructor
--
-- if we choose to lower record type
--
-- @
-- e.g. lower (a, i8, c) = struct {a, i8#, c}
--      lower { name: str, age: int } = struct { #[ptr (bit 8)], int32 }
-- @
--
-- In this case, we have advantage to have same structure, using which we can
-- transfrom it into graphic type and do type unification.
--
-- lets say we have a type:
--
-- @
--    A := forall a b. {name: str, age: int, identity: a, property: b}
--    B := forall a. {name: str, age: int, identity: a, property: likelyint a}
-- @
--
-- and we have toplevel definition:
--
-- @
--    type likelyint a = int
-- @
--
-- we will always have this:
--
-- @
--    lower (forall a. likelyint a) = int32 // if parameter "a" is locally bound
-- @
-- 
-- and we choose not to lowering record type :
--
-- @
--    lower A = forall a b. { name: #[ptr (bit 8)], age, int32, identity: a, property: b}
--    lower B = forall a. { name: #[ptr (bit 8)], age: int32, identity: a, property: int32 }
-- @
--
-- unify A and B we got:
--
-- @
--    unify (lower A) (lower B) = { name: #[ptr (bit 8)], age: int32, identity: int32, property: int32 }
-- @
-- 
-- and we can finally lower the type:
--
-- @
--    lower (unify (lower A) (lower B)) = struct {#[ptr (bit 8)], int32, int32, int32}
-- @
--
-- but remember signature for @lower (unify (lower A) (lower B))@ is still @Type bind rep name a@
-- and we need a final pass to lower it into @Rep Name@ which is something trivial. This kind of
-- @Rep Name@ thing is what we get finally to do codegen. and we can choose to transform
-- it into LLVM IR type or C type.
-----------------------------------------------------------------------------------------

-- | partially lowering type
lowerTypeRepPartial
  :: (f :<: rep, Traversable (bind name), Traversable rep, Monad m)
  => (forall x. f (m (AST.Type bind rep name x)) -> m (AST.Type bind rep name x))
  -> AST.Type bind rep name a -> m (AST.Type bind rep name a)
lowerTypeRepPartial handle = cata go
  where
    go TypPhtF = return TypPht
    go (TypVarF a) = return $ TypVar a
    go (TypConF m ms) = TypCon <$> m <*> sequence ms
    go (TypBndF fbind) = TypBnd <$> sequence fbind
    go (TypeF rma) =
      case prj rma of
        Just v -> handle v
        Nothing -> AST.Type . inj <$> sequence rma

lowerTypeRep
  :: (f :~: (h :+: g), Traversable g, Functor f, Traversable (bind name), Monad m)
  => (forall x. h (m (AST.Type bind g name x)) -> m (AST.Type bind g name x))
  -> AST.Type bind f name a -> m (AST.Type bind g name a)
lowerTypeRep handle = cata go
  where
    go TypPhtF = return TypPht
    go (TypVarF a) = return $ TypVar a
    go (TypConF m ms) = TypCon <$> m <*> sequence ms
    go (TypBndF fbind) = TypBnd <$> sequence fbind
    go (TypeF rma) = split handle (fmap AST.Type . sequence) rma

lowerTuple :: (f :~: (Tuple :+: g), Rep :<: g, Traversable g, Functor f, Traversable (bind name), Monad m)
           => AST.Type bind f name a -> m (AST.Type bind g name a)
lowerTuple = lowerTypeRep \(Tuple ms) -> forM ms (Embed . DataRep <$>) <&> AST.Type . inj . Rep . RepLift . struct False

lowerRecord
  :: (f :~: (Record Label :+: g), Rep :<: g, Traversable g, Functor f, Traversable (bind name), Monad m)
  => AST.Type bind f name a -> m (AST.Type bind g name a)
lowerRecord = lowerTypeRep \(Record (ms :: [(Label, m (AST.Type bind g name a))])) ->
  forM (fmap sequence ms) (Embed . DataRep . snd <$>) <&> AST.Type . inj . Rep . RepLift . struct False
