module Tlang.Emit
where

{-

Resolve type -> generate LLVM IR -> return the IR Module

-}

import Tlang.Codegen

import LLVM.Context (withContext)
import LLVM.Module (withModuleFromAST, moduleLLVMAssembly)
import LLVM.AST as AST hiding (function)
import LLVM.AST.Typed
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Type as AST

import LLVM.IRBuilder as IR hiding (ret, function)

import Data.Char (ord)
import qualified Data.ByteString.Char8 as C8 (putStrLn)
import Tlang.AST

import Control.Monad.Except
import Control.Monad.RWS

type CodegenEnv m = (MonadError String m, MonadState NameTable m, MonadReader NameTable m)
type LLVMEnv m = (MonadError String m, MonadState NameTable m)
type Codegen m = CodegenT NameTable NameTable String m

-- genModule :: AST.Module -> NameTable -> [ModuleElement ResolvedName TypAnno]
--           -> IO (Maybe (NameTable, AST.Module))
genModule m table eles = withContext $ \context -> do
  let run = runRWST . buildLLVM m emptyModuleBuilder
  result <- runExceptT $ run (mapM llvmGen (reverse eles)) () table
  case result of
    Left e -> putStrLn e >> return Nothing
    Right ((_, mod), ss, []) -> withModuleFromAST context mod $ \m -> do
      moduleLLVMAssembly m >>= C8.putStrLn  -- print out the llvm IR
      return $ Just (ss, mod)

-- llvmGen :: (LLVMEnv m, MonadFail m)
--         => ModuleElement ResolvedName TypAnno -> LLVM NameTable String m ()

llvmGen (FnD name typ body) =
  case body of
    FnDefault -> error "use c linkage to link external function synbol"
    FnSymbol sym -> error "link to external name with this"
    FnDecl lamb -> error "generate lambda"

-- lambdaGen :: CodegenEnv m => LambdaBlock ResolvedName -> Codegen m [String]
lambdaGen = error "not implemented"

-- Expression Generation
-- exprgen :: CodegenEnv m
--         => Expr (Operator String) ResolvedName -> Codegen m (Either [Operand] Operand)
exprgen (ExLit cVal) = undefined
    -- LitInt num -> return . Right $ IR.int32 num
    -- LitNumber num -> return . Right $ IR.double num
    -- LitString str -> do
    --   let val = IR.array $ fmap (AST.Int 8 . fromIntegral . ord) str ++ [AST.Int 8 0]
    --   storage <- IR.alloca (typeOf val) Nothing 1
    --   IR.store storage 0 val
    --   Right <$> IR.bitcast storage (AST.ptr AST.i8)
      -- Right <$> IR.load storage 0

