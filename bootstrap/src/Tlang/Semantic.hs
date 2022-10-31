module Tlang.Semantic
where

{-

This module resolves type for expression, function definition or declaration.

-}

import Tlang.Parser

import qualified Tlang.Type.Primitive as PrimTyp
import qualified Tlang.Type.AlgebraType as AlgeTyp

import Control.Monad.State

import Tlang.Type.Class (LLVMTypeConvert (..))
import LLVM.AST.Type (Type (..), ptr)
import LLVM.AST.Name (mkName)

data TypResolv = TypResPrim PrimTyp.PrimType -- primitive type
               | TypResRec (AlgeTyp.SimpleRecordType TypResolv) -- simple structure type for now
               | TypResArray TypResolv (Maybe Integer) -- direct array type is mapped into primitiveType
               | TypResArrow TypResolv TypResolv  -- simple function type
               | TypResAlia String  -- type name reference
               | TypResPtr TypResolv -- pointer type
               | TypResVoid
               deriving (Show, Eq)

instance LLVMTypeConvert TypResolv where
  llvmType (TypResPrim p) = llvmType p
  llvmType (TypResRec t) = llvmType t
  llvmType (TypResArray eleT len'maybe) =
    case ArrayType <$> fmap fromIntegral len'maybe of
      Nothing -> ArrayType 0 (llvmType eleT)
      Just f -> f $ llvmType eleT
  llvmType (TypResAlia name) = NamedTypeReference $ mkName name
  llvmType (TypResPtr t) = ptr $ llvmType t
  llvmType (TypResVoid) = VoidType
  llvmType arr = case flatArrowType arr of
                   Nothing -> undefined
                   Just ts -> let (paras, retyp) = (\ls -> (init ls, last ls)) $ llvmType <$> ts
                              in FunctionType retyp (filter (/= VoidType) paras) False
  -- TODO, i removed Void type from parameter list, but it is not covering every situation
  -- To solve this, we need a better type deduction algorighm and AST structure.

flatArrowType :: TypResolv -> Maybe [TypResolv]
flatArrowType (TypResArrow a1 a2) = do
  if isArr a2
  then do ts <- flatArrowType a2
          return $ a1:ts
  else return [a1, a2]
  where isArr :: TypResolv -> Bool
        isArr (TypResArrow _ _) = True
        isArr _ = False
flatArrowType _ = Nothing

type EnvState = [(String, TypResolv)]
data TypResolvEnv = TypResolvEnv
  { getTypesEnv :: EnvState
  , getTermsEnv :: EnvState
  } deriving (Show, Eq)

type TypResolvEnvState = (TypResolvEnv, TypResolvEnv)

newtype ResolvType a = ResolvType { runResolvType :: State TypResolvEnvState a }
  deriving (Functor, Applicative, Monad, MonadState TypResolvEnvState)

type Selector = TypResolvEnvState -> TypResolvEnv
local, global :: Selector
local = snd
global = fst
type Modifier = (TypResolvEnv -> TypResolvEnv) -> TypResolvEnvState -> TypResolvEnvState
localModifier, globalModifier :: Modifier
localModifier = fmap
globalModifier f = \(a, b) -> (f a, b)

lookupTerms, lookupTypes :: Selector -> String -> ResolvType (Maybe TypResolv)
lookupTerms s name = do
  env <- gets $ getTermsEnv . s
  return $ lookup name env
lookupTypes s name = do
  env <- gets $ getTypesEnv . s
  return $ lookup name env

modifyTypesEnv, modifyTermsEnv :: Modifier -> (EnvState -> EnvState) -> ResolvType ()
modifyTypesEnv s f = modify $ s $ \(TypResolvEnv typs terms) -> TypResolvEnv (f typs) terms
modifyTermsEnv s f = modify $ s $ \(TypResolvEnv typs terms) -> TypResolvEnv typs (f terms)

setEnv :: TypResolvEnv -> Modifier -> ResolvType ()
setEnv val s = modify $ s $ const val
resetEnv :: Modifier -> ResolvType ()
resetEnv = setEnv (TypResolvEnv [] [])

pushTermsEnv, pushTypesEnv :: Modifier -> (String, TypResolv) -> ResolvType ()
pushTermsEnv md val = modifyTermsEnv md $ \s -> val : s
pushTypesEnv md val = modifyTypesEnv md $ \s -> val : s

annoResolve :: TypAnno -> ResolvType (Either String TypResolv)
annoResolve (TypName name) = do
  prim'maybe <- lookupTypes global name
  case prim'maybe of
    Just typ -> case typ of
      TypResPrim _ -> pure . Right $ typ  -- return primitive type
      TypResVoid -> pure . Right $ typ
      _ -> pure . Right $ TypResAlia name -- return alias if it is not primitive
    Nothing -> pure . Left $ "Scala Type " ++ name ++ " doesn't exist!"
annoResolve (TypArrow a1 a2) = do
  a1'either <- annoResolve a1
  a2'either <- annoResolve a2
  let res'either = fmap (,) a1'either <*> a2'either
  return $ flip fmap res'either $ \(a, b) -> TypResArrow a b
annoResolve (TypApply _a1 _a2) = undefined -- TODO: finish this after introducing type variables
annoResolve (TypRec name fields) = do
  prim'maybe <- lookupTypes global name
  case prim'maybe of
    Just _ -> return . Left $ "RecordType " ++ name ++ " already exists!"
    Nothing -> do
      field'either <- fmap sequence $ flip mapM fields $ \(fieldName, fieldType) ->
        fmap (fieldName,) <$> annoResolve fieldType
      return $ TypResRec . AlgeTyp.consRecord (Just name) <$> field'either
annoResolve (TypArray anno len'maybe) = do
  typ'either <- annoResolve anno
  return $ TypResArray <$> typ'either <*> Right len'maybe
annoResolve (TypPtr anno) = do
  typ'either <- annoResolve anno
  return $ TypResPtr <$> typ'either

emptyResolvEnv :: TypResolvEnv
emptyResolvEnv = TypResolvEnv (compounds ++ prims) []
  where prims = fmap TypResPrim <$>
                [ ("bool",  PrimTyp.bool)
                , ("float", PrimTyp.float)
                , ("double",PrimTyp.double)
                , ("i8",    PrimTyp.int8)
                , ("i16",   PrimTyp.int16)
                , ("i32",   PrimTyp.int32)
                , ("i128",  PrimTyp.int128)
                , ("char",  PrimTyp.char)
                , ("short", PrimTyp.short)
                , ("long",  PrimTyp.long)
                , ("u8",    PrimTyp.uint8)
                , ("u16",   PrimTyp.uint16)
                , ("u32",   PrimTyp.uint32)
                , ("u64",   PrimTyp.uint64)
                , ("byte",  PrimTyp.byte)
                ]
        compounds = [ ("void", TypResVoid)
                    ]

execResolv :: ResolvType a -> (a, TypResolvEnv)
execResolv = fmap fst . flip runState (emptyResolvEnv, emptyResolvEnv) . runResolvType

-- Do Type annotation and some semantic analysis
resolve :: ModuleElement UntypedName TypAnno -> ResolvType (Either String (ModuleElement (TypedName TypResolv) TypAnno))
resolve (ModuleFunction (UntypedName name) retyp'maybe body'maybe) = do
  res'maybe <- lookupTerms global name
  res <- case res'maybe of
    Just nam -> return . Left $ "Duplicated Global Definition of " ++ name
    Nothing -> do
      anno'maybe'either <- fmap sequence . sequence $ annoResolve <$> retyp'maybe
      case body'maybe of
        Just body -> do
          body'either <- fmap join . sequence $ flip resolvLambda body <$> anno'maybe'either
          let buildFuncEffe = \block -> do
                let typ'either = collectType <$> anno'maybe'either <*> Right block
                flip mapM_ typ'either $ \typ -> pushTermsEnv globalModifier (name, typ)
                return $ buildFunc name <$> typ'either <*> Right (Just block)
          mapM buildFuncEffe body'either
        Nothing -> do
          let caseFunc Nothing = return . Left $ "Function Declaration must have type annotation"
              caseFunc (Just typ@(TypResArrow _ _)) = do
                pushTermsEnv globalModifier (name, typ)
                return . Right $ ModuleFunction (TypedFunc name typ) Nothing Nothing
              caseFunc (Just _) = return . Left $ "Function Declaration must have Arrow type annotation"
          mapM caseFunc anno'maybe'either
  resetEnv localModifier -- clear local environment
  return $ join res
  where
    buildFunc :: String -> TypResolv -> Maybe (LambdaBlock (TypedName TypResolv)) -> ModuleElement (TypedName TypResolv) TypAnno
    buildFunc name typ body = ModuleFunction (TypedFunc name typ) Nothing body
    resolvLambda :: Maybe TypResolv -> LambdaBlock UntypedName
                 -> ResolvType (Either String (LambdaBlock (TypedName TypResolv)))
    resolvLambda retyp (LambdaBlock vars exprs) = do
      -- TODO: check redundant parameter name
      vars'either <- fmap sequence $ flip mapM vars $ \(UntypedName name, anno'maybe) -> do
        case anno'maybe of
          Just anno -> do
            anno'either <- annoResolve anno
            let addLocalVar a = pushTermsEnv localModifier (name, a)
            mapM_ addLocalVar anno'either
            return $ (\a -> (TypedLocalVar name a, Nothing)) <$> anno'either
          Nothing -> return . Left $ "LambdaBlock Parameter " ++ name ++ " lacks type annotation"
      exprs'either <- sequence <$> mapM resolvExpr exprs
      return $ LambdaBlock <$> vars'either <*> exprs'either
    collectType :: Maybe TypResolv -> LambdaBlock (TypedName TypResolv) -> TypResolv
    collectType retyp (LambdaBlock names exprs) = case retyp of
        Just typ -> foldl1 TypResArrow $ typlst' ++ [typ]
        Nothing -> foldl1 TypResArrow $ typlst' ++ [TypResVoid]
      where typlst = snd . getTypedName . fst <$> names
            typlst' = if length typlst == 0 then [TypResVoid] else typlst

resolve (ModuleBinding (UntypedName name) anno'maybe val) = do
  -- TODO: check redundant binding
  expr'either <- resolvExpr val
  typ'either'maybe <- sequence $ annoResolve <$> anno'maybe
  case typ'either'maybe of
    Just (Left s) -> return . Left $ "Global Binding for " ++ name ++ " Failed: " ++ s
    Just (Right typ) -> do
      let bind = ModuleBinding (TypedGlobalVar name typ) Nothing
      pushTermsEnv globalModifier (name, typ)
      return $ bind <$> expr'either
    -- TODO, we force type annotation for binding now
resolve (ModuleType (UntypedName name) anno) = do
  typ'either <- annoResolve anno
  case typ'either of
    Left s -> return . Left $ "Type Definition for " ++ name ++ " Failed: " ++ s
    Right typ -> do
      pushTypesEnv globalModifier (name, typ)
      return . Right $ ModuleType (TypedTypeDec name typ) anno
resolve _ = undefined

resolvName :: String -> ResolvType (Either String (TypedName TypResolv))
resolvName name = lookupTerms local name >>= \local'maybe -> do
  case local'maybe of
    Nothing -> lookupTerms global name >>= \global'maybe -> do
      case global'maybe of
        Nothing -> return . Left $ "Can't find definition for variable " ++ name
        Just typ -> case typ of
          TypResArrow _ _ -> return . Right $ TypedFunc name typ
          _ -> return . Right $ TypedGlobalVar name typ
    Just typ -> return . Right $ TypedLocalVar name typ

-- data ConstraintResolv term typ =
--   NewConstraint [typ -> Bool] | SolvConstraint term typ (ConstraintResolv term typ)
--   deriving (Show, Eq)

-- TODO, every experssion should have type binding, otherwise we can't do type deduction
-- TODO, introduce constraint type resolve algorithm to find out value type
resolvExpr :: Expr UntypedName Op -> ResolvType (Either String (Expr (TypedName TypResolv) Op))
resolvExpr (ExLit lit) = pure . Right $ ExLit lit
resolvExpr (ExRef (UntypedName name)) = do
  name'either <- resolvName name
  return $ ExRef <$> name'either
resolvExpr (ExCall (UntypedName fname) exprs) = do
  name'either <- resolvName fname
  exprs'either <- sequence <$> mapM resolvExpr exprs
  return $ ExCall <$> name'either <*> exprs'either
resolvExpr (ExOp op e1 e2) = do
  e1'either <- resolvExpr e1
  e2'either <- resolvExpr e2
  return $ ExOp op <$> e1'either <*> e2'either

