module Tlang.Codegen
  ( module Core

  , NameTable

  , createModule
  , shortString

  , getvar
  , putvar
  -- , getGlobal
  -- , getLocal
  -- , function
  , ret
  )
where

import Control.Monad.RWS
import Tlang.Codegen.Core as Core

import LLVM.AST hiding (function)
import LLVM.AST.Constant
import LLVM.IRBuilder hiding (ret, function)
import LLVM.AST.Global
import LLVM.AST.Type

import Data.List (find)
import Data.ByteString.Short (toShort)
import Data.ByteString.Char8 (pack)

createModule :: String -> String -> Module
createModule label source = defaultModule
                          { moduleName = shortString label
                          , moduleSourceFileName = shortString source
                          }

shortString = toShort . pack


type NameTable = [(String, Operand)]

-- | get local value from name table
getvar :: MonadState NameTable m => String -> CodegenT r NameTable e m (Maybe Operand)
getvar n = gets $ fmap snd . find (\(name, _) -> name == n)

-- | add new variable to name table
putvar :: MonadState NameTable m => String -> Operand -> CodegenT r NameTable e m ()
putvar s v = modify ((s,v) :)

-- | access global name as reference
-- getGlobal :: Monad m => String -> Type -> CodegenT r s e m Operand
-- getGlobal (mkName -> name) typ = return . ConstantOperand $ GlobalReference typ name
-- | access local name as reference
-- getLocal :: Monad m => String -> Type -> CodegenT r s e m Operand
-- getLocal (mkName -> name) typ = return $ LocalReference typ name

-- | generate an end block with optional value
ret :: Monad m => Maybe Operand -> CodegenT r s e m ()
ret val = emitTerm $ Ret val []

-- | emit a function definition
-- function :: Monad m => Name -> Type -> [(Type, String)] -> [BasicBlock] -> LLVM s e m Operand
-- function name retty pairs blocks = do
--   let def = GlobalDefinition functionDefaults
--           { name = name
--           , parameters = ((\(ty, nm) -> Parameter ty (mkName nm) []) <$> pairs, False)
--           , returnType  = retty
--           , basicBlocks = blocks
--           }
--       funty = ptr $ FunctionType retty (fst <$> pairs) False
--   emitDefn def
--   return $ ConstantOperand $ GlobalReference funty name
