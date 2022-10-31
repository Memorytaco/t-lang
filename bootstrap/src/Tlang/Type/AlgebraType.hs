module Tlang.Type.AlgebraType

where

{-

This module build up abstraction of record type and sum type.

-}

import qualified Tlang.Type.Primitive as PrimT
import Tlang.Type.Class (LLVMTypeConvert (..))
import LLVM.AST.Type (Type (..))

data ConcretType
data SimpleSumType 

-- newtype SimpleTypeNameRef = SimpleTypeNameRef String deriving (Show, Eq)

-- SimpleRecordType doesn't allow direct self reference
data SimpleRecordType field =
  SimpleRecordType RecordTypeConsName [RecordTypeField field]
  deriving (Show, Eq)

instance LLVMTypeConvert field => LLVMTypeConvert (SimpleRecordType field) where
  llvmType (SimpleRecordType _ ts) = StructureType False $ llvmType <$> ts

instance LLVMTypeConvert field => LLVMTypeConvert (RecordTypeField field) where
  llvmType (RecordTypeField _ t) = llvmType t

getRecordName :: SimpleRecordType a -> Maybe String
getRecordName (SimpleRecordType name _) =
  case name of
    RecordTypeConsAnonymous -> Nothing
    RecordTypeConsName v -> Just v

maybeRecordName :: String -> SimpleRecordType a -> String
maybeRecordName s = maybe s id . getRecordName

consRecord :: Maybe String -> [(String, field)] -> SimpleRecordType field
consRecord name'maybe fields =
  let consField (name, field) = RecordTypeField (RecordTypeFieldName name) field
  in
  case name'maybe of
    Just name -> SimpleRecordType (RecordTypeConsName name) $ consField <$> fields
    Nothing -> SimpleRecordType RecordTypeConsAnonymous $ consField <$> fields

data RecordTypeConsName = RecordTypeConsAnonymous | RecordTypeConsName String deriving (Show, Eq)
data RecordTypeField a = RecordTypeField RecordTypeFieldName a deriving (Show, Eq)
newtype RecordTypeFieldName = RecordTypeFieldName String deriving (Show, Eq)
