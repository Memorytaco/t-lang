module Tlang.Type.Class
  ( TypeId (..)
  , LLVMTypeConvert (..)
  )
where

import qualified LLVM.AST.Type as AType (Type)
import LLVM.AST.AddrSpace (AddrSpace (..))


-- tag type with number when building up the type
class TypeId a where
  typeid :: a -> Integer
  typename :: a -> String

class LLVMTypeConvert a where
  llvmType :: a -> AType.Type

