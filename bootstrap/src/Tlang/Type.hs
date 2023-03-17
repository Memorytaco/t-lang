module Tlang.Type
  ( module Primitive
  , module Concrete

  , Type (..)
  , DataRep (..)
  , Concrete
  )
where

import Control.Applicative (Const)

import Tlang.Type.Primitive as Primitive
import Tlang.Type.Concrete as Concrete
import Tlang.AST

type Concrete name c f = ConcreteType SeqT (Type Label name () (Const Symbol) c f)

-- | transfer from `Type` to runtime `DataRep`, the core tech to support
-- polymorphic type
data DataRep = DataRep

-- instance (Show nam, LLVMTypeEncode t) => LLVMTypeEncode (Type k nam t) where
--   encode (OQuantified name _) = error $ "UnExpected quantified variable " <> show name
--   encode (OTypeAlias name va t) =
--     case va of
--       [] -> encode t
--       _ -> error $ "UnExpected type alias " <> show name <> " " <> intercalate " " (show <$> va)
--   encode (OTypeName name _) = flip T.PointerType (T.AddrSpace 0) $ T.NamedTypeReference (T.mkName $ show name)
--   encode (OApply _ _ _) = error $ "UnExpected type application of Type"
--   encode (OLabel _ _ t) = encode t
--   encode (OEnum _ _) = T.IntegerType 32
--   encode (OVariant name vs ts) =
--     case vs of
--       [] -> let isEnum (OEnum _ _) = True
--                 isEnum _ = False
--                 isEnumAll = and . fmap isEnum
--              in if isEnumAll ts then T.i32 else T.StructureType False $ T.i32: (encode <$> ts)
--       _ -> error $ "Unexpected type variable of " <> show vs <> " for " <> show name
--   encode (ORecord name vs t) =
--     case vs of
--       [] -> encode t
--       _ -> error $ "Unexpected type variable of " <> show vs <> " for " <> show name
--   encode (OLift t) = encode t

-- instance (LLVMTypeClass t) => LLVMTypeClass (Type k nam t) where
--   classOf (OQuantified _ _) = Aggregate
--   classOf (OTypeAlias _ _ t) = classOf t
--   classOf (OTypeName _ _) = Aggregate -- we don't know whether a typename is primitive or else, it should be replaced before checking
--   classOf (OApply _ _ _) = Aggregate
--   classOf (OLabel _ _ t) = classOf t
--   classOf (OEnum _ _) = Aggregate
--   classOf (OVariant _ _ _) = Aggregate
--   classOf (ORecord _ _ _) = Aggregate
--   classOf (OLift t) = classOf t
