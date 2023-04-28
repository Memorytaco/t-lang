module Tlang.Inference.Kind
  ( NameIndex
  , GraftKind
  , NormalKind
  , DeBruijnType
  , KindUnifyError (..)

  , toDebruijn
  , infer
  , kinding
  )
where

import Tlang.AST
import Tlang.Helper.AST
import Tlang.Constraint
import Tlang.Subst
import Tlang.Generic (prj)
import Tlang.Extension.Type as Ext

import qualified Data.Functor.Foldable as F (ListF (..))
import Control.Monad
import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState (..), modify, gets, runStateT, runState, StateT, execState)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Except (MonadError (..), ExceptT, runExcept, lift)
import Control.Applicative (Const (..))
import Control.Monad.Trans.RWS (evalRWS, RWS, RWST (..), tell, pass)
import Data.Bifunctor
import Data.Functor (($>), (<&>))
import Data.Functor.Foldable hiding (ListF (..))
import Data.List (union, find, nub)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

-- | indexed type variable.
-- we keep original symbol name for error message and recovery from deBruijn notation
type NameIndex = Either Symbol Integer

-- | kind annotation will be enclosed into Shift closure
-- typ1 : k1 == Closure typ1 ([k1] :! 0)
type GraftKind = Kind (Graft (MetaVar Kind Integer)) Integer
type NormalKind = Kind Identity Integer
-- | The `Context` keeps recording kind of variables, type variable index will keep a copy of
-- environment if it has a closure.
type Context = [GraftKind]
-- | The type is represented with deBruijn notation. Constraint will take effect over grafted type variable or index
-- - `r` is for any concrete type representation
type DeBruijnType inj rep = StandardType NameIndex Label (Bound Integer) inj rep

-- cookAll :: MonadFail m => [Symbol] -> [Symbol :== Type Label Symbol () None Identity r] -> m [Symbol :== DeBruijnType (TypeMetaVar GraftKind) (DefaultConstraint term) Identity r]
-- cookAll syms decs = cata go decs
--   where go F.Nil = return []
--         go (F.Cons (name :== typ) ms) = do
--           iTyp <- toDebruijn (Just name :== typ) (symbols <> syms, [])
--           (name :== iTyp :) <$> ms
--         symbols = fmap (\(a :== _) -> a) decs

assignNames :: forall a b m. MonadState Integer m
            => [a :== b] -> m [GraftKind :@ a :== b]
assignNames = cata go
  where go F.Nil = return []
        go (F.Cons typ ms) = do
          name <- freshName
          (first (:@ name) typ:) <$> ms
        freshName :: m GraftKind
        freshName = modify (+1) *> get <&> KindLift . Graft . MetaVar

-- | traverse into type structure and build the variable index. Also, it detects recursive type depending on whether it is named type.
toDebruijn
  :: forall v f r m. (MonadFail m, Traversable f)
  => (Maybe Symbol :== StandardType Symbol Label (Bound Symbol) f r) -- ^ It can lift named or unnamed type into deBruijn representation
  -> ([Symbol], [Symbol])   -- ^ type environment, contains type name. (global environment, local environment)
  -> m (DeBruijnType f r)   -- ^ return the standard representation
toDebruijn (name :== tree) = runReaderT (cata go tree)
  where
    go :: Base (StandardType Symbol Label (Bound Symbol) f r) (ReaderT ([Symbol], [Symbol]) m (DeBruijnType f r))
       -> ReaderT ([Symbol], [Symbol]) m (DeBruijnType f r)
    go TypPhtF = pure TypPht
    go (TypRepF r) = pure $ TypRep r
    go (TypRefF s) = do
      ix'maybe <- asks $ lookup s . flip zip [1..] . snd
      genv <- asks fst
      case Right <$> ix'maybe of
        Nothing -> case (s `elem` genv, name == Just s) of
                     (_, True) -> return . TypRef $ Left s
                     (True, _) -> return . TypRef $ Left s
                     (False, False) ->
                       fail $ "type variable " <> show s <> " doesn't occur in scop"
        Just v -> return $ TypRef v
    go (TypLetF binder mt) = do
      let handler = handleAll <$> (prj binder) <|> handleAbs <$> (prj binder)
      case handler of
        Just m -> m
        Nothing -> fail $ "Unrecognized binder"
      where
        handleAll (Forall bound) = handleBound (injTypeBind @(Forall (Bound Integer)) . Forall) bound mt
        handleAbs (Scope bound) = handleBound (injTypeBind @(Scope (Bound Integer)) . Scope) bound mt
    go (TypLitF lit) = do
      let handler = handleSum <$> (prj lit) <|> handleRec <$> (prj lit) <|> handleTup <$> (prj lit) <|> handleLit <$> (prj lit)
      case handler of
        Just m -> m
        Nothing -> fail $ "Unrecognized literal"
      where
        handleSum (Variant trs) = injTypeLit @(Variant Label) . Variant <$> forM trs \(field, mt) -> (field,) <$> sequence mt
        handleRec (Record trs) = injTypeLit @(Record Label) . Record <$> forM trs \(field, mt) -> (field,) <$> mt
        handleTup (Tuple rs) = injTypeLit @Tuple . Tuple <$> sequence rs
        handleLit (Const v) = pure . injTypeLit @(Const Ext.Literal) $ Const v
    go (TypConF mt mtn) = TypCon <$> mt <*> sequence mtn
    go (TypInjF fmt) = TypInj <$> sequence fmt
    handleBound f bound mt =
      case bound of
        s :> mr -> do
          boundType <- mr
          local (second (s:)) $ f (1 :> boundType) <$> mt
        s :~ mr -> do
          boundType <- mr
          local (second (s:)) $ f (1 :~ boundType) <$> mt

-- | remove meta variable and generalize it.
generalize :: GraftKind -> NormalKind
generalize kind = fst $ evalRWS monad 0 []
  where
    monad :: RWS Integer () [Integer] NormalKind
    monad = do
      flip cata kind \case
        KindTypeF -> return ()
        KindRefF _ -> return ()
        KindLiftF (Graft s) -> modify ([getMetaVar s] `union`)
        KindLiftF (UnGraft mk) -> mk
        KindAbsF _ mk -> mk
        mk1 ::>$ mk2 -> mk1 >> mk2
      index :: [(Integer, Integer)] <- gets $ flip zip [1..]
      typeRoh :: NormalKind <- flip cata kind \case
        KindTypeF -> return KindType
        KindLiftF (Graft (MetaVar var)) -> do
          level <- ask
          case lookup var index of
            Just i -> return $ KindRef (i + level)
            Nothing -> error "can't find index of variable"
        KindLiftF (UnGraft mk) -> mk
        KindRefF i -> return $ KindRef i
        KindAbsF _ mk -> local (+1) $ mk <&> KindAbs 1
        mk1 ::>$ mk2 -> (::>) <$> mk1 <*> mk2
      return $ quantify (length index) typeRoh
      where
        quantify 0 = id
        quantify n = KindAbs 1 . quantify (n - 1)

instance {-# InCoherent #-} Grafting (MetaVar Kind Integer :-> GraftKind) GraftKind where
  graft (v :-> rt) = cata \case
    KindTypeF -> KindType
    KindRefF name -> KindRef name
    KindAbsF name t -> KindAbs name t
    t1 ::>$ t2 -> t1 ::> t2
    KindLiftF (UnGraft t) -> t
    KindLiftF (Graft i) -> if i == v then rt else KindLift (Graft i)

-- | accept a initial seed for meta name, return the lifted kind and the new seeds
expand :: Integer -> NormalKind -> (GraftKind, Integer)
expand seed = flip runState seed . cata \case
  KindTypeF -> pure KindType
  KindRefF i -> pure $ KindRef i
  KindAbsF _ mk -> do
    k <- mk
    v <- modify (+1) >> gets (KindLift . Graft . MetaVar)
    return $ rewrite k (Cons () v (() :! 0))
  mk1 ::>$ mk2 -> (::>) <$> mk1 <*> mk2
  KindLiftF (Identity mk) -> mk

instGraft :: MonadState Integer m => (MetaVar Kind Integer :-> NormalKind) -> GraftKind -> m GraftKind
instGraft (ix :-> k)  = cata \case
  KindTypeF -> return KindType
  KindRefF i -> return $ KindRef i
  KindAbsF name mk -> KindAbs name <$> mk
  mk1 ::>$ mk2 -> (::>) <$> mk1 <*> mk2
  KindLiftF (UnGraft mk) -> mk
  KindLiftF (Graft v) ->
    if v == ix
    then do
      seed <- get
      let (gk, seed2) = expand seed k
      put seed2
      return gk
    else return $ KindLift (Graft v)

instGraft' :: MonadState Integer m => (GraftKind :-> NormalKind) -> GraftKind -> m GraftKind
instGraft' (KindLift (Graft v) :-> k) = instGraft (v :-> k)
instGraft' _ = return

instConst' :: MonadState Integer m => (GraftKind :-> NormalKind) -> [GraftKind :<> GraftKind] -> m [GraftKind :<> GraftKind]
instConst' subst = cata \case
  F.Nil -> return []
  F.Cons (a :<> b) ms -> do
    x <- (:<>) <$> instGraft' subst a <*> instGraft' subst b
    (x:) <$> ms

data KindUnifyError
  = KindUnMatch GraftKind GraftKind
  | InvalidRefIndex Integer GraftKind
  | InvalidPolyKind
  -- | wrong implementation of algorithm or recursive constraint
  | InvalidMetavariablePosition (MetaVar Kind Integer) [MetaVar Kind Integer :-> GraftKind]
  deriving (Show, Eq)

unify :: forall m. Monad m => [GraftKind :<> GraftKind] -> ExceptT KindUnifyError m [MetaVar Kind Integer :-> GraftKind]
unify = checkOccurrence . unifySubst <=< refold shrink rollup
  where
    rollup [] = F.Nil
    rollup ((a1 ::> b1) :<> (a2 ::> b2) :xs) = rollup $ [a1 :<> a2, b1 :<> b2] <> xs
    rollup (x :xs) = F.Cons x xs
    detect :: (GraftKind :<> GraftKind) -> ExceptT KindUnifyError m [MetaVar Kind Integer :-> GraftKind]
    detect (KindLift (UnGraft k1) :<> k2) = detect (k1 :<> k2)
    detect (k1 :<> KindLift (UnGraft k2)) = detect (k1 :<> k2)

    detect (KindLift (Graft v) :<> k@(KindLift (Graft v2))) = if v == v2 then pure [] else pure [v :-> k]
    detect (KindLift (Graft v) :<> k) = pure [v :-> k]
    detect (k :<> KindLift (Graft v)) = pure [v :-> k]

    detect (KindType :<> KindType) = pure []
    detect (k1@KindType :<> k2) = throwError $ KindUnMatch k1 k2
    detect (k1 :<> k2@KindType) = throwError $ KindUnMatch k1 k2

    detect (KindRef i :<> k) = throwError $ InvalidRefIndex i k
    detect (k :<> KindRef i) = throwError $ InvalidRefIndex i k
    detect (KindAbs _ _ :<> _) = throwError InvalidPolyKind
    detect (_ :<> KindAbs _ _) = throwError InvalidPolyKind
    detect ((a1 ::> b1) :<> (a2 ::> b2)) = do
      r1 <- detect (a1 :<> a2)
      r2 <- detect (b1 :<> b2)
      return $ r1 <> r2

    shrink F.Nil = pure []
    shrink (F.Cons (k1 :<> k2) ts) = do
      ls <- ts
      a <- detect (graft ls k1 :<> graft ls k2)
      return $ a <> ls

    unifySubst :: [MetaVar Kind Integer :-> GraftKind] -> [MetaVar Kind Integer :-> GraftKind]
    unifySubst sust = execState (go sust) []
      where
        go :: MonadState [MetaVar Kind Integer :-> GraftKind] m1
           => [MetaVar Kind Integer :-> GraftKind] -> m1 [MetaVar Kind Integer :-> GraftKind]
        go [] = pure []
        go (x:xs) = do
          let ulist = fmap (fmap $ graft x)
          s <- get
          put (x: ulist s)
          go $ ulist xs

    -- | when a meta variable occurs on both side of substitution, it indicates either an error in the unifying
    -- algorithm or a lack of potential higher rank annotation for type
    -- checkOccurrence :: [MetaVar Kind Integer :-> GraftKind] -> m [MetaVar Kind Integer :-> GraftKind]
    checkOccurrence vs =
      let (as, bs) = foldl folder ([], []) vs
       in mapM_ (check as) bs $> vs
      where folder (as, bs) (a :-> b) = (a:as, b:bs)
            check vars = cata \case
              KindTypeF -> return ()
              KindRefF _ -> return ()
              KindAbsF _ mk -> mk
              (mk1 ::>$ mk2) -> mk1 >> mk2
              KindLiftF (Graft mv) -> when (mv `elem` vars) . throwError $ InvalidMetavariablePosition mv vs
              KindLiftF (UnGraft mk) -> mk

-- | traverse around annotated type tree
onAnnotate :: (anno1 -> anno2) -> DeBruijnType ((:@) anno1) r -> DeBruijnType ((:@) anno2) r
onAnnotate f = cata \case
  TypPhtF -> TypPht
  TypRepF r -> TypRep r
  TypLitF lit -> TypLit lit
  TypRefF r -> TypRef r
  TypLetF b t -> TypLet b t
  TypConF r rn -> TypCon r rn
  TypInjF (r :@ rk) -> TypInj (r :@ f rk)

-- | unsolved and solved deBurijn type with arbitrary constraint
type GroundEnv r =
  ( [(GraftKind :@ Symbol) :== DeBruijnType Identity r]
  , [(NormalKind :@ Symbol) :== DeBruijnType ((:@) NormalKind) r]
  )

-- | kinding problem with name
infer :: forall r m. (MonadFail m, MonadState (GroundEnv r) m)
      => (GraftKind :@ Symbol :== DeBruijnType Identity r) -> Integer
      -> m (Integer, DeBruijnType ((:@) NormalKind) r)
infer (name :@ sig :== tree) i = do
  env <- let mp = fmap \(a :== b) -> a in gets $ bimap ((name :@ sig :) . mp) mp
  lenv <- gets fst

  let lookupSym a = find (\(b :@ _ :== _) -> a == b)
      gen t i = runRWST (genConstraint (pure . runIdentity) t) env ([], i)
      loopGen :: [GraftKind :@ Symbol] -> Integer
              -> StateT ([Symbol], [GraftKind :@ Symbol :== DeBruijnType ((:@) GraftKind) r], [GraftKind :<> GraftKind]) m Integer
      loopGen [] ix = return ix
      loopGen ((nm :@ nsig) :xs) ix = do
        let (_ :== rtyp) = fromJust $ lookupSym nm lenv
        (nexts, nnix) <- do
          ((cs, typ :@ k), (_, nix), inits) <- lift $ gen rtyp ix
          if null inits || inits == [nm :@ nsig]
          then do
            update <- fmap generalize . graft <$> unify' (nsig :<> k :cs)
            -- this symbol is already solved, no more constraints added, but we need to update
            -- existed constraint using `instGraft`.
            modify \(syms, typs, ls) -> (nm:syms, typs, ls)
            lift . modify $ fmap (nm :@ update nsig :== onAnnotate update typ :)
            (constraints, nnix) <- gets (\(_,_,a) -> a) >>= flip runStateT nix . instConst' (nsig :-> update nsig)
            modify \(a,b,_) -> (a, b, constraints)
            return (xs, nnix)
          else do
            modify \(syms, typs, ls) -> (nm:syms, (nm :@ nsig :== typ):typs, nsig :<> k : cs <> ls)
            syms <- gets (\(syms, _, _) -> filter (\(sym :@ _) -> sym `notElem` syms) $ inits `union` xs)
            return (syms, nix)
        loopGen nexts nnix

  ((cs, typ :@ k), (_, ni), inits) <- gen tree i
  (nix, (syms, typs, constraints)) <- runStateT (loopGen (filter (\(sym :@ _) -> sym /= name) inits) ni) ([name], [], sig :<> k :cs)

  unifier <- unify' constraints
  let update = generalize . graft unifier
  -- error $ show typ <> " ===== " <> show unifier <> "  ...   " <> show (constraints <> [sig :<> k])
  forM_ ((name :@ sig :== typ) :typs) \(sym :@ ssig :== rtyp) -> do
    modify $ fmap (sym :@ update ssig :== onAnnotate update typ :)
  modify . first $ filter (\(s :@ _ :== _) -> s `notElem` syms)
  return (nix, onAnnotate update typ)

  where
    unify' :: MonadFail n => [GraftKind :<> GraftKind] -> n [MetaVar Kind Integer :-> GraftKind]
    unify' = either (fail . show) return . runExcept . unify

-- failure example:
-- data F f g a = (f g * f a * g a) ;;

-- | kinding problem with no type name
kinding :: forall r m. (MonadFail m, MonadReader [NormalKind :@ Symbol] m)
        => (DeBruijnType Identity r, Integer)
        -> m (DeBruijnType ((:@) NormalKind) r, Integer)
kinding (tree, i) = do
  env <- ask
  ((constraints, typ :@ k), (_, ni), []) <- runRWST (genConstraint (pure . runIdentity) tree) ([], env) ([], i)
  unifier <- either (fail . show) return $ runExcept (unify constraints)
  let update = generalize . graft unifier
  return (onAnnotate update typ, ni)

-- | parameters for RWST are
-- 1. (symbol to be solved, symbol solved already)
-- 2. mutual meta variable
-- 3. local context
type ConstraintMonad m a = RWST ([GraftKind :@ Symbol], [NormalKind :@ Symbol]) [GraftKind :@ Symbol] (Context, Integer) m a
type AnnotatedType r = GraftKind :@ DeBruijnType ((:@) GraftKind) r

genConstraint
  :: forall r m f. (MonadFail m, Traversable f)
  -- allow to choose whatever effect we would like to handle different shape of `TypInj`
  => (f (AnnotatedType r) -> ConstraintMonad m (AnnotatedType r))
  -> DeBruijnType f r -- ^ the unsolved type term
  -> ConstraintMonad m ([GraftKind :<> GraftKind], AnnotatedType r)
genConstraint natural = pass . fmap (, nub) . cata go
  where
    -- | new meta kind variable
    freshName :: MonadState (Context, Integer) mf => mf GraftKind
    freshName = modify (fmap (+1)) >> get <&> KindLift . Graft . MetaVar . snd
    -- | a synonym to annotate type term
    annotate term typ = TypInj (term :@ typ) :@ typ
    -- | create a local term environment for type abstraction
    stack :: ConstraintMonad m a -> GraftKind -> ConstraintMonad m a
    stack ma v = do
      s1 <- gets fst
      modify (first (v:)) >> ma <* modify (first $ const s1)
    {- collect kind signature constraint from type constraint
    -- stripConstraint = cata \case
    --         SatisfiableF b -> (Satisfiable b, [])
    --         AndF lr rr -> (And (fst lr) (fst rr), snd lr <> snd rr)
    --         ProjectF n (r, s) -> (Project n r, s)
    --         PredicateF (Rigid ((t1 :@ _, s1) :<> a)) -> (Predicate (Rigid $ t1 :<> a), s1)
    --         PredicateF (Flexible ((t1 :@ _, s1) :< a)) -> (Predicate (Flexible $ t1 :< a), s1)
    --         InstantiateF t (t1 :@ _, s) -> (Instantiate t t1, s)
    --         IntroduceF t (t1 :@ _, s1) (r, s2) -> (Introduce t t1 r, s1 <> s2)
    -}
    {- find symbol in environment and record unsolved signature
    -}
    lookupSymbol :: Symbol -> ConstraintMonad m GraftKind
    lookupSymbol sym = do
      (lenv, renv) <- ask
      case find (\(s :@ _) -> s == sym) renv of
        Just (_ :@ nk) -> do
          ix <- gets snd
          let (gk, ni) = expand ix nk
          modify (second $ const ni)
          return gk
        Nothing -> case find (\(s :@ _) -> s == sym) lenv of
          -- collect unsolved term signature, prepared for solving mutual recursive type term
          Just r@(_ :@ k) -> tell [r] >> return k
          Nothing -> fail $ "Can't find signature for " <> show sym

    go :: (a ~ ([GraftKind :<> GraftKind], AnnotatedType r))
       => Base (DeBruijnType f r) (ConstraintMonad m a) -> ConstraintMonad m a
    go TypPhtF = return . pure $ TypPht `annotate` KindType
    go (TypRepF r) = return . pure $ TypRep r `annotate` KindType
    go (TypLitF litr) = do
      let handler = handleTup <$> (prj litr) <|> handleRec <$> (prj litr) <|> handleSum <$> (prj litr) <|> handleLit <$> (prj litr)
      case handler of
        Just m -> m
        Nothing -> fail $ "Unknown literal"
      where
        handleTup (Tuple rm) = do
          (s, rs) <- foldl (\(ss, ts) (as, term :@ k) -> (ss <> (KindType :<> k : as), ts <> [term])) mempty <$> sequence rm
          return (s, injTypeLit @Tuple (Tuple rs) :@ KindType)
        handleRec (Record rm) = do
          (s, rs) <- foldl (\(ss, ts) (l, (s, term :@ k)) -> (ss <> (k :<> KindType : s), ts <> [(l,term)])) mempty <$> mapM sequence rm
          return (s, injTypeLit @(Record Label) (Record rs) :@ KindType)
        handleSum (Variant rm) = do
          (s, rs) <- foldl collectField mempty <$> forM rm (mapM sequence)
          return (s, injTypeLit @(Variant Label) (Variant rs) :@ KindType)
            where
              collectField (ss, ts) (l, Nothing) = (ss, ts <> [(l, Nothing)])
              collectField (ss, ts) (l, Just (s, t :@ k)) = (ss <> (k :<> KindType : s), ts <> [(l, Just t)])
        handleLit (Const lit) = return . pure $ injTypeLit @(Const Ext.Literal) (Const lit) `annotate` KindType  -- FIXME: add kind literal
    go (TypLetF binder mt) = do
      let handler = handleAbs <$> (prj binder) <|> handleAll <$> (prj binder)
      case handler of
        Just m -> m
        Nothing -> fail "Unknown binder"
      where
        handleAbs (Scope nm) = do
          n <- freshName
          (s, name) <- mapM (fmap \(t :@ _) -> t) <$> stack (sequence nm) n
          (subst, term :@ k) <- stack mt n
          return (s <> subst, injTypeBind @(Scope (Bound Integer)) (Scope name) term `annotate` (n ::> k))
        handleAll (Forall nm) = do
          n <- freshName
          (s, name) <- mapM (fmap \(t :@ _) -> t) <$> stack (sequence nm) n
          (subst, term :@ k) <- stack mt n
          return (s <> (k :<> KindType : subst), injTypeBind @(Forall (Bound Integer)) (Forall name) term `annotate` KindType)

    -- go (TypPieF cm mt) = do
    --   n <- freshName
    --   (s, c) <- mapM (fmap \(t :@ _) -> t) <$> stack (sequence cm) n
    --   (subst, term :@ k) <- stack mt n
    --   return (s <> (k :<> KindType : subst), TypPie c term `annotate` KindType)

    go (TypRefF v@(Right i)) = do
      item <- gets $ (!! fromInteger (i - 1)) . fst
      return . pure $ TypRef v `annotate` item

    {- single recursive type is explicitly marked since it doesn't complicate the kind inference process.
    -- same technique used as for mutual recursive type.
    -- a single recursive type is only possible for named type unless we introducing explicit recursive type aka. mu type.
    -}
    {- for mutual recursive type, we attach new generated metavariable to all unsolved type, and keep recording the constraint
    -- and unify the whole constraints at some point.
    -}
    go (TypRefF v@(Left sym)) = lookupSymbol sym >>= \sig -> return . pure $ TypRef v `annotate` sig
    go (TypConF mt mtn) = do
      (s1, term1 :@ k1) <- mt
      ((sn, kn), termn) <- mapM (\(s, t :@ k) -> ((s, [k]), t)) <$> sequence mtn
      k <- freshName
      return ((k1 :<> foldr (::>) k kn) : s1 <> sn, TypCon term1 termn `annotate` k)
    go (TypInjF fmt) = do
      (s, ft) <- sequence <$> sequence fmt
      (s,) <$> natural ft
