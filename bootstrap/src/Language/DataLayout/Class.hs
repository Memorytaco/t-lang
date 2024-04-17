module Language.DataLayout.Class
  ( TypeMangle (..)
  , EncodeLLVMType (..)

  , LLVMTypeClass (..)
  , TypeClass (..)

  , TypeMangleError (..)
  )
where

import qualified LLVM.AST.Type as T (Type)

-- | TODO: find proper error of type encod
data TypeMangleError = TypeMangleError deriving (Show, Eq)

--
class TypeMangle a where
  mangle :: a -> String
  demangle :: String -> Either TypeMangleError a

-- encode type to LLVM IR lowlevel type.
class EncodeLLVMType a where
  encodeLLVMType :: a -> T.Type

-- | used to help choosing vector or list in llvm IR
data TypeClass = Primitive | Aggregate deriving (Show, Eq)
class LLVMTypeClass a where
  classOf :: a -> TypeClass
