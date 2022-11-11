module Tlang.Emit
where

{-

Resolve type -> generate LLVM IR -> return the IR Module

-}

import Tlang.Parser
import Tlang.Codegen
import Tlang.Semantic
import Tlang.Type.Class (LLVMTypeConvert (..))
import Tlang.Type.Polymorphism

import LLVM.Context (withContext)
import LLVM.Module (withModuleFromAST, moduleLLVMAssembly)
import LLVM.AST as AST hiding (function)
import LLVM.AST.Typed
import qualified LLVM.AST.Name as AST
import qualified LLVM.AST.Constant as AST
import qualified LLVM.AST.Type as AST

import LLVM.IRBuilder as IR hiding (ret, function)

import Data.Char (ord)
import qualified Data.ByteString.Char8 as C8 (putStrLn)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.RWS
import Data.Functor.Identity (Identity)
import Data.List (find)

type ResolvedName = TypedName (SymbolString TypResolv)
type CodegenEnv m = (MonadError String m, MonadState NameTable m, MonadReader NameTable m)
type LLVMEnv m = (MonadError String m, MonadState NameTable m)
type Codegen m = CodegenT NameTable NameTable String m

genModule :: AST.Module -> NameTable -> [ModuleElement ResolvedName TypAnno] -> IO (Maybe (NameTable, AST.Module))
genModule m table eles = withContext $ \context -> do
  let run = runRWST . buildLLVM m emptyModuleBuilder
  result <- runExceptT $ run (mapM llvmGen (reverse eles)) () table
  case result of
    Left e -> putStrLn e >> return Nothing
    Right ((_, mod), ss, []) -> withModuleFromAST context mod $ \m -> do
      moduleLLVMAssembly m >>= C8.putStrLn  -- print out the llvm IR
      return $ Just (ss, mod)

llvmGen :: (LLVMEnv m, MonadFail m) => ModuleElement ResolvedName TypAnno -> LLVM NameTable String m ()

llvmGen (ModuleFunction name _ Nothing) = do
  case name of
    TypedOnly _ -> throwError "Codegen: An external function should have a name!"
    TypedName name typ -> do
      t <- getLLVMType typ
      case t of
        AST.FunctionType r as _ -> do
          def <- IR.extern (AST.mkName name) as r
          modify ((name, def):)
        _ -> throwError "An external function should have arrow type"
      return ()

llvmGen (ModuleFunction nam _ (Just lambda)) = do
  (name, typ) <- case nam of
    TypedOnly _ -> throwError "Function Definition should have name"
    TypedName a b -> return (a, b)
  native <- getLLVMType typ
  (retty, paraty) <- case native of
                       AST.FunctionType r as _ -> return (r, as)
                       r -> return (r, [])
  definitions <- get
  ((names, blks), []) <- lift $ evalRWST ((runCodegen . lambdaGen) lambda  emptyIRBuilder) definitions []
  def <- function name retty (zip paraty names) blks
  modify ((name, def):)
  return ()
llvmGen _ = undefined

lambdaGen :: CodegenEnv m => LambdaBlock ResolvedName -> Codegen m [String]
lambdaGen (LambdaBlock (fmap fst -> vars) es) = do
  IR.ensureBlock
  names <- forM vars \case
    TypedOnly _ -> throwError "Lambda Parameter should have name"
    TypedName name ltyp -> do
      typ <- getLLVMType ltyp
      storage <- IR.alloca typ Nothing 4
      val <- getLocal name typ
      IR.store storage 4 val
      lval <- IR.load storage 0
      putvar name lval
      return name
  e'exprs <- forM es exprgen
  case e'exprs of
    [] -> ret Nothing
    (last -> a) -> case a of
                     Left _ -> throwError "Partial function application is not supported"
                     Right v -> ret (Just v)
  return names

-- Expression Generation
exprgen :: CodegenEnv m
        => Expr Op ResolvedName -> Codegen m (Either [Operand] Operand)
exprgen (ExLit (getTypedNameT -> _typ) cVal) = case cVal of
    LitInt num -> return . Right $ IR.int32 num
    LitNumber num -> return . Right $ IR.double num
    LitString str -> do
      let val = IR.array $ fmap (AST.Int 8 . fromIntegral . ord) str ++ [AST.Int 8 0]
      storage <- IR.alloca (typeOf val) Nothing 1
      IR.store storage 0 val
      Right <$> IR.bitcast storage (AST.ptr AST.i8)
      -- Right <$> IR.load storage 0

exprgen (ExRef (TypedOnly _)) = throwError "Expression Name Don't have any reference"
exprgen (ExRef (TypedName name _typ)) = do
  definitions <- ask
  value <- getvar name
  case value of
    Just v -> return $ Right v
    Nothing -> case lookup name definitions of
      Nothing -> throwError $ "Couldn't find value of variable " <> name
      Just v -> return . Right $ v

exprgen (ExCall (getTypedNameT -> typ) e1 e2) = do
  op1 <- either id (:[]) <$> exprgen e1
  op2 <- join $ either (const $ throwError "Lambda application is not supported") pure <$> exprgen e2
  let ops = op1 <> [op2]
  case typ of
    UnSolved v -> throwError $ "Wrong TypeChecking, Found one UnSolved type variable " <> show v
    Solved _ -> let (callee, vars) = (head ops, tail ops)
                 in Right <$> IR.call callee ((, []) <$> vars)
    (_ :-> _) -> return $ Left ops

exprgen (ExOp op (getTypedNameT -> _typ) opa opb) =
  case Map.lookup op (fst opstb) of
    Just opf -> do
      let err = throwError "Ilegal Operand type, Lambda is not supported now"
      a <- join $ either (const err) pure <$> exprgen opa
      b <- join $ either (const err) pure <$> exprgen opb
      Right <$> opf a b
    Nothing -> throwError "Unsupported operator"
    where
      opstb :: Monad m
            => ( Map.Map Op (Operand -> Operand -> Codegen m Operand)
               , Map.Map Op (Operand -> Operand -> Codegen m Operand))
      opstb = ( Map.fromList
                [ (OpAdd,     IR.fadd)
                , (OpMinus,   IR.fsub)
                , (OpMulti,   IR.fmul)
                , (OpDivide,  IR.fdiv)
                ]
                , Map.fromList
                [ (OpAdd,     IR.add)
                , (OpMinus,   IR.sub)
                , (OpMulti,   IR.mul)
                -- , (OpDivide,  IR.div)
                ]
              )

exprgen _ = undefined
