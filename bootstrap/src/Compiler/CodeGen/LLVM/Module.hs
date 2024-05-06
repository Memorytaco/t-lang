module Compiler.CodeGen.LLVM.Module

where


import LLVM.AST
    ( defaultModule, Module(moduleSourceFileName, moduleName) )

import Data.ByteString.Short (toShort, ShortByteString)
import Data.ByteString.Char8 (pack)

data LLVMModuleTag = LLVMModuleTag
  { labelLLVMModuleTag :: String
  , sourceNameLLVMModuleTag :: String
  } deriving (Show, Eq, Ord)

createModule :: LLVMModuleTag -> Module
createModule (LLVMModuleTag label source) =
  defaultModule
  { moduleName = shortString label
  , moduleSourceFileName = shortString source
  }

shortString :: String -> ShortByteString
shortString = toShort . pack

