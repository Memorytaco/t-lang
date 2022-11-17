module Tlang.Analysis.Stage1 where

{-

This module resolves type for expression, function definition or declaration.

-}

import Tlang.Parser
import Tlang.Parser.Pratt
import Tlang.Type.Class (LLVMTypeConvert (..))
import Tlang.Type.Checker

import qualified Tlang.Type.Primitive as PrimTyp
import qualified Tlang.Type.AlgebraType as AlgeTyp

import Control.Monad.State

import LLVM.AST.Type (Type (..), ptr)
import LLVM.AST.Name (mkName)

import Control.Monad.Except
import Data.Bifunctor (bimap)
import Control.Monad.RWS.Strict as RWS
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (find, intercalate)

import Data.String (IsString (..))
newtype VName = VName String deriving (Eq, IsString)
instance Show VName where
  show (VName v) = v

type SymbolString typ = Symbol VName typ

data TypResolv = TypResPrim PrimTyp.PrimType -- primitive type
               | TypResRec (AlgeTyp.SimpleRecordType TypResolv) -- simple structure type for now
               | TypResArray TypResolv (Maybe Integer) -- direct array type is mapped into primitiveType
               | TypResName String  -- type name reference
               | TypResPtr TypResolv -- pointer type
               | TypResVoid
               deriving (Eq)

instance Show TypResolv where
  show (TypResPrim v) = show v
  show (TypResRec v) = show v
  show (TypResArray a _) = "[" <> show a <> "]"
  show (TypResName name) = "&" <> name
  show (TypResPtr a) = "ref " <> show a
  show TypResVoid = "void"

instance LLVMTypeConvert TypResolv where
  llvmType (TypResPrim p) = llvmType p
  llvmType (TypResRec t) = llvmType t
  llvmType (TypResArray eleT len'maybe) =
    case ArrayType <$> fmap fromIntegral len'maybe of
      Nothing -> ArrayType 0 (llvmType eleT)
      Just f -> f $ llvmType eleT
  llvmType (TypResName name) = NamedTypeReference $ mkName name
  llvmType (TypResPtr t) = ptr $ llvmType t
  llvmType TypResVoid = VoidType

type TopSubstitution = ([VName :|-> SymbolString TypResolv], [String :|-> SymbolString TypResolv])

type ResolvTypeContext r w s m = RWS.RWST r w s (InferContextT m)
type ResolvTypeT e m = ResolvTypeContext TopSubstitution [String] () (ExceptT e m)
type ResolvType e m = ResolvTypeT e m

annoResolve :: forall m. Monad m => TypAnno -> ResolvType String m (SymbolString TypResolv)
annoResolve = cata go
  where
    go :: Base TypAnno (ResolvType String m (SymbolString TypResolv))
       -> ResolvType String m (SymbolString TypResolv)
    go (TypNameF name) = do
      (typs, _) <- ask
      return $ sigma typs (UnSolved $ VName name)
    go (TypPtrF w) = do
      -- Don't support pointer for arrow, TODO here. pointer will be introduced with parametric type sys.
      -- to Resolve this, the pointer wrapping and unwrapping is done in next codegen stage.
      sym <- w
      case sym of
        Solved typ -> return . Solved $ TypResPtr typ
        UnSolved _ -> throwError $ "Unknown typ for annotation ptr " <> show sym
        _ -> throwError $ "Unknown typ for annotation ptr " <> show sym
    go (TypArrowF r1 r2) = do
      t1 <- r1
      t2 <- r2
      return (t1 :-> t2)
    go (TypArrayF anno len'maybe) = do
      typ <- anno
      (typs, _) <- ask
      case sigma typs typ of
        Solved t -> return . Solved $ TypResArray t len'maybe
        _ -> throwError $ "Unknown type element for annotation Array: " <> show typ
    go _ = undefined -- Type application and record type will be introduced in the future

preTypeDef :: [VName :|-> SymbolString TypResolv]
preTypeDef = (\(name, typ) -> fromString name :|-> Solved typ) <$> (prism ++ compounds)
  where prism = fmap TypResPrim <$>
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
                    , ("str", TypResPtr . TypResPrim $ PrimTyp.char)
                    ]

-- environment is used to handle toplevel definition
-- When the name appears here, it means the code generation is done for them.
-- The only thing left to do is to generate code to refer to the name based
-- on their type. (e.g. transform arrow type to function pointer with closure object)
-- the environment is a substitution table from String (Name) to type.
moduleEnvironment :: TopSubstitution
moduleEnvironment = (preTypeDef, [])

runResolvT :: ResolvTypeContext env w rstate m term
          -> env  -- envrionment, provide basic type definition and term definition
          -> rstate -- maybe we want to manage something? for now it is ()
          -> InferContextTContext -- see `runInferContext`
          -> ([String :=| TypResolv], Int) -- see `runInferContext`
          -> m ( (GammaContext VName TypResolv :|- ((term, rstate, w) :| ConstraintContext VName TypResolv))
               , ([String :=| TypResolv], Int)
               )
runResolvT m env rstate = runInferContext (runRWST m env rstate)

execResolv :: forall ele m w
            . ( ele ~ ModuleElement (TypedName (SymbolString TypResolv)) TypAnno
              , MonadError String m
              )
           => ResolvTypeContext TopSubstitution w () m ele
           -> TopSubstitution
           -> Int
           -> m ((TopSubstitution, Int), ele)
execResolv ma env counter = do
  ((_ :|- ((term, _, _) :| _)), (_, ncounter)) <- runResolvT ma env () ([], []) ([], counter)
  let newEnv :: ele -> TopSubstitution -> m TopSubstitution
      newEnv (ModuleFunction tname _ _) (types, terms) =
        case tname of
          TypedOnly _ -> throwError "Function lacks name"
          TypedName name typ -> return (types, (name :|-> typ) :terms)
      newEnv (ModuleBinding tname _ _) e = undefined
      newEnv (ModuleType tname _) e = undefined
      newEnv _ _ = undefined
  nenv <- newEnv term env
  return ((nenv, counter), term)

evalResolv :: Monad m
           => TopSubstitution -- top level environment with type information
           -> Int -- counter
           -> String  -- content to parse
           -> ExceptT String m ( (TopSubstitution, Int)
                               , [ModuleElement (TypedName (SymbolString TypResolv)) TypAnno])
evalResolv env c s = do
  result <- liftEither . bimap show id $ parseToplevel s
  let connect m f = do
        ((env, c), ls) <- m
        fmap (:ls) <$> f env c
  foldl connect (pure ((env, c), [])) $ execResolv . resolve <$> result


-- lift untyped term into a system to process it and keeps all information untouched and let it be returned.
-- if we have some principal types, it returns with no error and caller need to put into more information to
-- resolve the principal type. Otherwise, it reports error with EitherT.
resolve :: forall m. Monad m
        => ModuleElement UntypedName TypAnno
        -> ResolvType String m (ModuleElement (TypedName (SymbolString TypResolv)) TypAnno)
resolve (ModuleFunction (UntypedName name) retyp'maybe body'maybe) = do
  (_, gBindings :: [String :|-> SymbolString TypResolv]) <- ask
  case find (\(n :|-> _) -> n == name) gBindings of
    Just _ -> throwError $ "Duplicated Global Definition of " ++ name
    Nothing -> do
      anno'maybe <- mapM annoResolve retyp'maybe
      case body'maybe of
        Just body -> do
          (block, tls) <- resolvLambda body
          let retT = getBodyRet block
              fullTyp = foldr1 (:->) (tls <> [retT])
          forM_ anno'maybe \anno ->
            when (anno /= retT) do
              throwError $ "Annotated return type of function "
                        <> name <> ": " <> show anno <> " doesn't match actual type " <> show retT
          return $ ModuleFunction (TypedName name fullTyp) Nothing (Just block)
        Nothing -> case anno'maybe of
          Nothing -> throwError "Function Declaration must have type annotation"
          (Just sym@(_ :-> _)) -> do
            let frees = freeVars sym
            when (frees /= []) do
              throwError $ "Types in Function Declaration have unResolved types: " <> show sym
            return $ ModuleFunction (TypedName name sym) Nothing Nothing
          (Just _) -> throwError "Function Declaration must have Arrow type annotation"
  where
    getBodyRet :: (LambdaBlock (TypedName (SymbolString TypResolv))) -> (SymbolString TypResolv)
    getBodyRet (LambdaBlock _ exs) = case getExprName <$> exs of
                                       [] -> Solved TypResVoid
                                       as -> case last as of
                                               TypedOnly t -> t
                                               TypedName _ t -> t
    resolvLambda :: LambdaBlock UntypedName
                 -> ResolvType String m
                    ((LambdaBlock (TypedName (SymbolString TypResolv))), [SymbolString TypResolv])
    resolvLambda (LambdaBlock vars exprs) = do
      varsR <- forM vars \case
        (UntypedName varName, anno'maybe) -> case anno'maybe of
            Just anno -> do
              sym <- annoResolve anno
              ((( paras :: [VName := SymbolString TypResolv] ), _), _)<- lift get
              case filter (\(a := _) -> a == VName varName) paras of
                [] -> lift $ modify \((bindings, a), b) -> (((VName varName := sym) :bindings, a), b)
                ss -> throwError $ "Existed Binding for " <> varName <> ": " <> show sym  <> " with " <> show ss
              return $ TypedName varName sym
            Nothing -> lift do
              nName <- newSymbolName
              modify \((bindings, a), b) -> (((VName varName := UnSolved nName) :bindings, a), b)
              return $ TypedName varName (UnSolved nName)
        (UntypedMarker, _) -> throwError $ "Internal Parser Error: Lambda Parameter has no name"

      -- construct expression
      env@(typs, _) <- ask
      exprsR <- forM exprs $ lift . inferExpr typs . reConstructExpr env

      -- We don't have parametric polyphism now, so types for binding should be resolved fully at this point
      (((syms :: [VName := SymbolString TypResolv]), _), _) <- lift get
      let unSolved = filter (\(_ := ls) -> length ls /= 0) $ fmap freeVars <$> syms
          errMsg = intercalate ", " . fmap (\(a := b) -> "{" <> show a <> "}" <> " has unResolved types " <> show b)
          lookupType :: TypedName a -> Maybe (TypedName (SymbolString TypResolv))
          lookupType (TypedOnly _) = Nothing
          lookupType (TypedName (VName -> vname) _) = (\(n := t) -> TypedName (show n) t) <$> find (\(n := _) -> vname == n) syms

      case unSolved of
        [] -> return ()
        as -> throwError $ errMsg as

      -- We don't support lambda now, so return type should not be arrow type
      case snd <$> exprsR of
        [] -> return ()
        as -> case last as of
                v@(_ :-> _) -> throwError
                  $ "Return type of " <> show v <> " is not allowed, Lambda is not supported now"
                _ -> return ()

      -- update types for parameters
      vs <- case mapM lookupType varsR of
        (Just vs) -> return vs
        Nothing -> throwError "Something Wrong"

      return (LambdaBlock ((,Nothing) <$> vs) (fst <$> exprsR), getTypedNameT <$> vs)

resolve (ModuleBinding (UntypedName name) anno'maybe val) = do
  -- TODO: check redundant binding
  env <- ask
  (e, _sym) <- lift $ reConstructExpr env val
  typ'maybe <- mapM annoResolve anno'maybe `catchError` \err -> do
    throwError $ "Global Binding for " ++ name ++ " Failed: " ++ err
  case typ'maybe of
    Just typ -> do
      return $ ModuleBinding (TypedName name typ) Nothing e
    Nothing -> undefined -- TODO, we force type annotation for binding now

resolve (ModuleType (UntypedName name) anno) = do
  typ <- annoResolve anno `catchError` \e -> do
    throwError $ "Type Definition for " ++ name ++ " Failed: " ++ e
  return $ ModuleType (TypedName name typ) anno
resolve _ = undefined

newtype InferContextT m a = InferContextT
  { getInferContextT :: UnifyContextT VName TypResolv (StateT ([String :=| TypResolv], Int) m) a
  } deriving (Functor, Applicative, Monad)

type InferContextTContext =
    ( [VName := SymbolString TypResolv] -- var bindings
    , [SymbolString TypResolv := SymbolString TypResolv]  -- constraints
    )
type InferContextTState =
  ( InferContextTContext
  , ( [String :=| TypResolv]  -- possible solutions for a type var
    , Int                     -- free name tracker, used to index `variables`
    )
  )

-- a helper to get results from an infer context, it includes
-- 1. the whole context for solving type constraints
-- 2. the whole possible solution for some type variables
--    (e.g. literal 1 may have float or int type depends on its context)
-- 3. the tracker for free variables.
--    We use an indexed number and an infinite variable list to track variables now.
--    and We may expose it in the future to support different free variables tracking strategies.
runInferContext :: InferContextT m term -> InferContextTContext -> ([String :=| TypResolv], Int)
                -> m ( (GammaContext VName TypResolv :|- (term :| ConstraintContext VName TypResolv))
                     , ([String :=| TypResolv], Int)
                     )
runInferContext m ctx s = runStateT (unWrapUnifyContextT (getInferContextT m) ctx) s

instance MonadTrans InferContextT where
  lift ma = InferContextT . lift $ lift ma
instance MonadError String m => MonadError String (InferContextT m) where
  throwError = InferContextT . lift . lift . throwError
  catchError = undefined -- FIXME: we are unable to define it use only transformers

instance Monad m => MonadState InferContextTState (InferContextT m) where
  get = InferContextT do
    ctx <- get
    solutions <- lift get
    return (ctx, solutions)
  put (ctx, solutions) = InferContextT do
    put ctx
    lift $ put solutions

type InferExprRes = (Expr (Operator String) (TypedName (SymbolString TypResolv)), SymbolString TypResolv)

-- return concret type or a principal type
inferExpr :: (MonadError String m)
          => [VName :|-> SymbolString TypResolv] -- type substitution helper
          -> InferContextT m InferExprRes -> InferContextT m InferExprRes
inferExpr helper ma = do
  (e, sym) <- ma
  (bindings, constraints) <- fst <$> get
  solution <- unify constraints
  let unifier = sigma (solution <> helper)
      newConstraints = reduce solution constraints      -- keep recursive variables
      newBindings = fmap unifier <$> bindings           -- substitute types for bindings
  modify \(_, a) -> ((newBindings, newConstraints), a)
  if newConstraints == []
     then return (cata (onExprNameF (fmap unifier)) e, unifier sym)
     else throwError $ "Infinite type is not supported now: " <> show newConstraints

-- choose proper type for two different branches
phi :: MonadError String m
    => SymbolString TypResolv -> SymbolString TypResolv
    -> InferContextT m (SymbolString TypResolv)
phi (Solved a) (Solved b) =
  case a == b of
    True -> return $ Solved a
    False -> lift . throwError $ "BranchLeft " <> show a <> ", BranchRight " <> show b <> ", They should be same"
phi (UnSolved n) a = do
  InferContextT . addConstraint $ (UnSolved n := a)
  return (UnSolved n)
phi a (UnSolved n) = do
  InferContextT . addConstraint $ (UnSolved n := a)
  return (UnSolved n)
phi a@(Solved _) b@(_ :-> _) = lift . throwError $ "A scala type would never match an arrow type: " <> show a <> " !== " <> show b
phi a@(_ :-> _) b@(Solved _) = lift . throwError $ "A scala type would never match an arrow type: " <> show a <> " !== " <> show b
phi (a :-> b) (c :-> d) = do
  r1 <- phi a c
  r2 <- phi b d
  return $ r1 :-> r2

newSymbolName :: Monad m => InferContextT m VName
newSymbolName = do
  (_, ix) :: ([String :=| TypResolv], Int) <- snd <$> get
  modify . fmap $ fmap (+1)
  return $ fromString (variables !! ix)

-- build up envrionment for type inference
reConstructExpr :: (MonadError String m)
                => TopSubstitution -> Expr (Operator String) UntypedName -> InferContextT m InferExprRes
reConstructExpr env (ExLit _ lit) = do
  -- TODO: add value polymorphism
  let sym = case lit of
        LitInt _ -> Solved $ TypResPrim PrimTyp.int32
        LitNumber _ -> Solved $ TypResPrim PrimTyp.double
        LitString _ -> Solved . TypResPtr . TypResPrim $ PrimTyp.char
  return (ExLit (TypedOnly sym) lit, sym)
reConstructExpr (_, terms) (ExRef (UntypedName name)) = do
  sym'maybe <- InferContextT $ findBindingBy (== VName name)
  case sym'maybe of
    Just (_ := sym) -> return (ExRef (TypedName name sym), sym)
    Nothing -> case find (\(n :|-> _) -> n == name) terms of
                 Just (_ :|-> sym) -> return (ExRef (TypedName name sym), sym)
                 Nothing -> throwError $ "No available variable named " <> name

reConstructExpr env (ExCall _ expr1 expr2) = do
  (e1, sym1) <- inferExpr [] $ reConstructExpr env expr1
  (e2, sym2) <- inferExpr [] $ reConstructExpr env expr2
  tname <- newSymbolName
  InferContextT $ addConstraint (sym1 := (sym2 :-> UnSolved tname))
  return (ExCall (TypedOnly $ UnSolved tname) e1 e2, UnSolved tname)
reConstructExpr env (ExOp op _ expr1 expr2) = do
  (e1, sym1) <- inferExpr [] $ reConstructExpr env expr1
  (e2, sym2) <- inferExpr [] $ reConstructExpr env expr2
  -- to deal with this kind of situation
  -- we introduce one new type variable (T)
  -- and add two new constraint:
  --  T := A && T := B
  -- so later we will know to solve A and B first and
  -- check whether does A :== B holds.
  -- we are trying our best to find the most general type which
  -- is the principal type.
  sym <- phi sym1 sym2
  return (ExOp op (TypedOnly sym) e1 e2, sym)
reConstructExpr _ _ = undefined -- TODO

testResolve m = runResolvT m moduleEnvironment () ([], []) ([], 0)
testInfer m = runInferContext m ([], []) ([], 0)

playTypeChecker :: Monad m
                => String
                -> m (Either String [ModuleElement (TypedName (SymbolString TypResolv)) TypAnno])
playTypeChecker = runExceptT . fmap snd . evalResolv moduleEnvironment 0

data SimpleTree typ = Leaf typ | Branch [SimpleTree typ] deriving (Show, Eq, Functor)
$(makeBaseFunctor ''SimpleTree)

-- try to find native type of one symbol
getLLVMType :: forall m. (MonadError String m) => SymbolString TypResolv -> m Type
getLLVMType sym = do
  tree <- cata go sym
  return $ cata reduceType tree
  where
    reduceType :: Base (SimpleTree Type) Type -> Type
    reduceType (LeafF t) = t
    reduceType (BranchF ts) = FunctionType (last ts) (init ts) False

    go :: Base (SymbolString TypResolv) (m (SimpleTree Type))
       -> m (SimpleTree Type)
    go (UnSolvedF n) = throwError $ "UnSolved type: " <> show n
    go (SolvedF t) = return . Leaf $ llvmType t
    go (a1 :->$ a2) = do
      t1 <- a1
      t2 <- a2
      case t2 of
        Leaf _ -> return $ Branch [t1, t2]
        Branch ls -> return . Branch $ t1: ls
