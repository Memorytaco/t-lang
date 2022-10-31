module Tlang.Type.Class
  ( TypeId (..)
  , LLVMTypeConvert (..)
  )
where

import qualified LLVM.AST.Type as AType (Type)
import LLVM.AST.AddrSpace (AddrSpace (..))


-- tag type with number when building up the type, not useful for now.
class TypeId a where
  typeid :: a -> Integer
  typename :: a -> String

-- encode type to LLVM IR lowlevel type.
class LLVMTypeConvert a where
  llvmType :: a -> AType.Type

