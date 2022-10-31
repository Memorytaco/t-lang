module Tlang.Emit
where

{-

Resolve type -> generate LLVM IR -> return the IR Module

-}

import Tlang.Parser as TParser
import Tlang.Codegen
import qualified Tlang.Semantic as Semantic
import Tlang.Type.Class (LLVMTypeConvert (..))

import qualified Data.Map as Map
import Data.Char (ord)
import LLVM.AST as AST hiding (args)
import qualified LLVM.AST.Constant as Constant
import qualified LLVM.AST.Float as AFloat (SomeFloat(Double))
import LLVM.Context (withContext)
import LLVM.Module (withModuleFromAST, moduleLLVMAssembly)
import Data.ByteString.Char8 as C8 (putStrLn)
import qualified LLVM.AST.Type as AType

import Control.Monad.Except

type TypedSemanticName = TParser.TypedName Semantic.TypResolv

genModule :: AST.Module -> [TParser.ModuleElement (TParser.TypedName Semantic.TypResolv) TParser.TypAnno] -> IO AST.Module
genModule transmod fns = withContext $ \context -> withModuleFromAST context ast $ \m -> do
  moduleLLVMAssembly m >>= C8.putStrLn  -- print out the llvm IR
  return ast
      where ast = runLLVM transmod $ mapM codegen fns
            -- liftError :: ExceptT String IO a -> IO a
            -- liftError = runExceptT >=> either fail return

-- execResolv

codegen :: TParser.ModuleElement (TParser.TypedName Semantic.TypResolv) TParser.TypAnno -> LLVM ()
codegen (ModuleFunction nameT _ (Just lambdaBody)) =
  defineFunction (Left name) (fmap mkName <$> args, exprType) body
  where
    -- FIXME: ignore resolved type here, we need to handle relation between native type in llvm and our type system
    (name, _) = getTypedName nameT
    (args, expres) = lambdaPartialGen lambdaBody
    (exprType, genResult) = execCodegen $ do
        newBlock_ "entry"
        forM_ args $ \(typ, pname) -> do
          (_typ', var) <- alloca typ
          store var (localNameRef typ $ mkName pname)
          -- FIXME: decide what type to assign to named variable
          assign pname typ var
          return (pname, typ)
        vals <- forM expres exprgen
        if length vals == 0
        then ret Nothing >> return AType.void
        else do _ <- ret . Just $ last (fmap snd vals)
                return $ last (fmap fst vals)
    body :: [BasicBlock]
    body = transCodegenBlocks genResult
codegen (ModuleFunction nameT _ Nothing) =
  let (name, Just typs) = Semantic.flatArrowType <$> getTypedName nameT
   in declareFunction (Left name) (llvmType <$> init typs, llvmType $ last typs) Nothing

codegen (ModuleBinding nameT _ val) = do
  let (name, typ'unresolve) = getTypedName nameT
      (val'typ'unresolv, initialize) = fmap getConstant . fst $ execCodegen (exprgen val)
  defineGlobalVars (Left name) (llvmType typ'unresolve) True initialize
    where getConstant (ConstantOperand a) = Just a
          getConstant _ = Nothing

codegen (ModuleType nameT _) = do
  let (name, typ'unresolve) = getTypedName nameT
  declareType (mkName name) (Just $ StructureType False [llvmType typ'unresolve])

codegen _ = undefined

lambdaPartialGen :: LambdaBlock TypedSemanticName -> ([(Type, String)], [Expr TypedSemanticName Op])
lambdaPartialGen (LambdaBlock vars exprs) = (varMap <$> fmap fst vars, exprs)
    where
      varMap :: TypedSemanticName -> (Type, String)
      varMap (TypedLocalVar name typ) = (llvmType typ, name)


exprgen :: Expr TypedSemanticName Op -> Codegen (Type, Operand)
exprgen (ExLit cVal) = case cVal of
    LitInt num -> return (AType.i32, ConstantOperand $ Constant.Int 32 num)
    LitNumber num -> return (AType.double, ConstantOperand $ Constant.Float (AFloat.Double num))
    LitString str -> do
      let typ = AType.ArrayType ((1+) . fromIntegral . length $ str) AType.i8
          val = ConstantOperand $ Constant.Array AType.i8
                                $ fmap (Constant.Int 8 . fromIntegral . ord) str ++ [Constant.Int 8 0]
      (val'typ, storage) <- alloca typ
      assign "Local.String" val'typ storage
      store storage val
      return (val'typ, storage)

exprgen (ExRef nam) = do
  (typ, var) <- getvar (fst $ getTypedName nam)
  load typ var
exprgen (ExCall nameT exprs) = do
  let (name, typ) = getTypedName nameT
      Just retyps = fmap llvmType <$> Semantic.flatArrowType typ
  largs <- mapM exprgen exprs
  if length largs /= (length retyps - 1)
    then error "Number of Parameter doesn't match with function signature"
    else do
      safeArgs <- uncurry bitcast `mapM` zip (init retyps) (snd <$> largs)
      call (last retyps) (globalNameRef (AType.ptr $ llvmType typ) (mkName name)) (snd <$> safeArgs)
exprgen (ExOp op opa opb) = case Map.lookup op opstb of
    Just opf -> do
      (typa, ca) <- exprgen opa
      (typb, cb) <- exprgen opb
      if typa == typb then opf typa ca cb else opf typb ca cb
    Nothing -> error "unsupported operator"
    where opstb = Map.fromList
                  [ (OpAdd, fadd)
                  , (OpMinus, fsub)
                  , (OpMulti, fmul)
                  , (OpDivide, fdiv)
                  ]
