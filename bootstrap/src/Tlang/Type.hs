module Tlang.Type
  ( module Primitive
  , module Concrete

  , SystemF (..)
  , SystemOmega (..)
  , SystemOmegaF (..)
  , TypeKind (..)
  , KindT (..)
  , SimpleType
  )
where

import Tlang.Type.Primitive as Primitive
import Tlang.Type.Concrete as Concrete
import Data.Functor.Foldable.TH
import Tlang.Type.Class (LLVMTypeEncode (..), TypeEquality (..), LLVMTypeClass (..), TypeClass (..))
import Data.List (intercalate)
import qualified LLVM.AST as T
import qualified LLVM.AST.AddrSpace as T
import qualified LLVM.AST.Type as T

type SimpleType k nam = ConcreteType SeqT (SystemOmega (KindT k) nam)

-- A System F definition, with forall notation. TODO
data SystemF nam t =
    FQuantified nam -- ^ Introduce type variable
  | FNewType nam t  -- ^ Introduce type Alias
  | FLabel nam t     -- ^ Introduce Term Constructor, both for variant type and record type
  | FVariant nam [SystemF nam t]  -- ^ A toplevel variant definition, record type is simply a `Struct` in `Tlang.Type.Primitive`
  deriving (Show, Eq)

data KindT e = KindType | KindVar e deriving (Eq)
instance Show e => Show (KindT e) where
  show KindType = "*"
  show (KindVar e) = show e

-- A Naive definition for type kind system
data TypeKind k = KConcrete k | KPartial (TypeKind k) (TypeKind k) deriving (Eq)

instance Show k => Show (TypeKind k) where
  show (KConcrete k) = show k
  show (KPartial a b) = show a <> " -> " <> show b

-- A System FΩ definition. with Higher Kinded Type Support. TODO
data SystemOmega k nam t =
    OQuantified nam (TypeKind k) -- ^ Normal type variable
  | OTypeAlias nam [nam] (SystemOmega k nam t)  -- ^ Type alias
  | OTypeName nam Bool -- ^ Type name reference, with a recursive marker, True means recursive and false means non recursive
  | OApply (TypeKind k) (SystemOmega k nam t) (SystemOmega k nam t)  -- ^ Type Application, which means the first one should at least has kind * -> *
  | OLabel nam Integer (SystemOmega k nam t)    -- ^ Label for Record and Variant
  | OEnum nam Integer -- label branch with no additional type
  | OVariant nam [nam] [SystemOmega k nam t]  -- ^ TopLevel Variant definition
  | ORecord nam [nam] t -- ^ TopLevel Record definition
  | OLift t -- ^ Lift type up into this level
  deriving (Eq)

$(makeBaseFunctor ''SystemOmega)

instance (Show t, Show k, Show nam) => Show (SystemOmega k nam t) where
  show (OQuantified name k) = "∀" <> show name <> " :: " <> show k
  show (OTypeAlias name va t) = "@" <> show name <> " " <> intercalate " " (show <$> va) <> " = " <> show t
  show (OTypeName nam b) = (if b then "μ" else "") <> show nam
  show (OApply k t1 t2) = "(" <> show t1 <> " " <> show t2 <> " :: " <> show k <> ")"
  show (OLabel name _ t) = show name <> " = " <> show t
  show (OEnum name _) = show name
  show (OVariant name vars ts) = show name <> " " <> intercalate " " (fmap show vars) <> " {" <> intercalate "; " (fmap show ts) <> "}"
  show (ORecord name vars t) = show name <> " " <> intercalate " " (fmap show vars) <> show t
  show (OLift t) = show t

instance (Show nam, LLVMTypeEncode t) => LLVMTypeEncode (SystemOmega k nam t) where
  encode (OQuantified name _) = error $ "UnExpected quantified variable " <> show name
  encode (OTypeAlias name va t) =
    case va of
      [] -> encode t
      _ -> error $ "UnExpected type alias " <> show name <> " " <> intercalate " " (show <$> va)
  encode (OTypeName name _) = flip T.PointerType (T.AddrSpace 0) $ T.NamedTypeReference (T.mkName $ show name)
  encode (OApply _ _ _) = error $ "UnExpected type application of Type"
  encode (OLabel _ _ t) = encode t
  encode (OEnum _ _) = T.IntegerType 32
  encode (OVariant name vs ts) =
    case vs of
      [] -> let isEnum (OEnum _ _) = True
                isEnum _ = False
                isEnumAll = and . fmap isEnum
             in if isEnumAll ts then T.i32 else T.StructureType False $ T.i32: (encode <$> ts)
      _ -> error $ "Unexpected type variable of " <> show vs <> " for " <> show name
  encode (ORecord name vs t) =
    case vs of
      [] -> encode t
      _ -> error $ "Unexpected type variable of " <> show vs <> " for " <> show name
  encode (OLift t) = encode t

instance (LLVMTypeClass t) => LLVMTypeClass (SystemOmega k nam t) where
  classOf (OQuantified _ _) = Aggregate
  classOf (OTypeAlias _ _ t) = classOf t
  classOf (OTypeName _ _) = Aggregate -- we don't know whether a typename is primitive or else, it should be replaced before checking
  classOf (OApply _ _ _) = Aggregate
  classOf (OLabel _ _ t) = classOf t
  classOf (OEnum _ _) = Aggregate
  classOf (OVariant _ _ _) = Aggregate
  classOf (ORecord _ _ _) = Aggregate
  classOf (OLift t) = classOf t

instance
  ( Eq k, Eq nam, TypeEquality t t, TypeEquality (SystemOmega k nam t) t
  ) => TypeEquality (SystemOmega k nam t) (SystemOmega k nam t) where
    OLift a ==? OLift b = a ==? b
    OLift a ==? b = b ==? a
    a ==? OLift b = a ==? b
    OQuantified name'a k'a ==? OQuantified name'b k'b = k'a == k'b && name'a == name'b
    OTypeAlias name'a va a ==? OTypeAlias name'b vb b = name'a == name'b && va == vb && (a ==? b)
    OTypeName name'a a ==? OTypeName name'b b = name'a == name'b && a == b
    OApply k'a a1 a2 ==? OApply k'b b1 b2 = k'a == k'b && (a1 ==? b1) && (a2 ==? b2)
    OLabel name'a _ a ==? OLabel name'b _ b = name'a == name'b && (a ==? b)
    OEnum name'a _ ==? OEnum name'b _ = name'a == name'b
    OVariant name'a va as ==? OVariant name'b vb bs =
      and $ [va == vb, name'a == name'b, length as == length bs] <> fmap (uncurry (==?)) (zip as bs)
    ORecord name'a va a ==? ORecord name'b vb b = name'a == name'b && va == vb && (a ==? b)
    _ ==? _ = False

instance
  ( TypeEquality (t (BaseType t (ConcreteType t (SystemOmega k n))))
                 (t (BaseType t (ConcreteType t (SystemOmega k n))))
  , Eq k, Eq n
  ) => TypeEquality (SystemOmega k n (ConcreteType t (SystemOmega k n))) (ConcreteType t (SystemOmega k n)) where
    OLift a ==? Lift b = a ==? b
    OLift a ==? Field b = b ==? a
    a ==? Field b = a ==? b
    a ==? Lift b = a ==? b

instance
  ( TypeEquality (t (BaseType t (ConcreteType t (SystemOmega k n))))
                 (t (BaseType t (ConcreteType t (SystemOmega k n))))
  , Eq k, Eq n
  ) => TypeEquality (SystemOmega k n (ConcreteType t (SystemOmega k n))) (BaseType t (ConcreteType t (SystemOmega k n))) where
    OLift a ==? Extend b = a ==? b
    OLift a ==? b = a ==? b
    _ ==? _ = False
