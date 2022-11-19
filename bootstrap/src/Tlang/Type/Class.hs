module Tlang.Type.Class
  ( TypeMangle (..)
  , LLVMTypeEncode (..)
  , TypeEquality (..)

  , LLVMTypeClass (..)
  , TypeClass (..)

  , TypeMangleError (..)
  )
where

import qualified LLVM.AST.Type as T (Type)
import LLVM.AST.AddrSpace (AddrSpace (..))

-- | TODO: find proper error of type encod
data TypeMangleError = TypeMangleError deriving (Show, Eq)

--
class TypeMangle a where
  mangle :: a -> String
  demangle :: String -> Either TypeMangleError a

-- encode type to LLVM IR lowlevel type.
class LLVMTypeEncode a where
  encode :: a -> T.Type

data TypeClass = Primitive | Aggregate deriving (Show, Eq)

class LLVMTypeClass a where
  classOf :: a -> TypeClass

class TypeEquality a b where
  (==?) :: a -> b -> Bool
