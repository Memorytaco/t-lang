module Tlang.Inference.Type
  ( localKind
  , cookExpr
  , cookPattern
  , cookLambda
  , nameRef
  , opRef
  , localRef
  , sortKey

  , clean
  , scopUnify
  , unify
  , typing
  , runTyping
  , bindCheck

  , NormalType
  , PatternError
  )
where

{-

This module resolves type for expression, function definition or declaration.

-}

import Tlang.AST
import Tlang.Helper.AST
import Tlang.Subst (Rule (..), Subst (..))
import Tlang.Extension.Type as Ext
import Tlang.Generic (prj)

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.State
import Control.Monad.Identity (Identity (..))
import Control.Monad.RWS (RWST (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Data.Functor ((<&>))
import Data.List (sortBy)
import Data.Set (fromList)
import Data.Bifunctor (first, bimap)

import Data.Functor.Foldable.TH
import Data.Functor.Foldable

import qualified Tlang.Subst as S (Subst (..))

import Tlang.Parser.Type (ParseType)
import Tlang.Parser.Expr (ParseExpr, ParseExprType, ParseLambda, ParseLambdaType)
import Tlang.Parser.Pattern (ParsePattern, ParsePatternType)

import Tlang.Inference.Kind (kinding, DeBruijnType, NormalKind, toDebruijn)

-- | we use `()` to mark some named type is a primitive one and later transformed it into
-- the real primitive type.
type NormalType k r = DeBruijnType ((:@) k) r

type CookEnv c m
  = ( Monad m
    , MonadFail m
    , MonadReader [NormalKind :@ Symbol] m)

localKind :: CookEnv c m => ParseType -> m (NormalType NormalKind (StandardRepType Symbol Label (Bound Symbol) Identity))
localKind typ = do
  env <- ask
  ntyp <- toDebruijn (Nothing :== typ) (fmap (\(a :@ _) -> a) env, [])
  ftyp <- fst <$> runReaderT (kinding (ntyp, 0)) env
  return ftyp

-- | prepare the expression form to be solved, along with pattern and lambda.
cookExpr
  :: CookEnv c m
  => ParseExprType -> m (ParseExpr (NormalType NormalKind (StandardRepType Symbol Label (Bound Symbol) Identity)))
cookExpr = cata go
  where
    go ExUnitF = return ExUnit
    go (ExSelF l) = return $ ExSel l
    go (ExLitF lit) = return $ ExLit lit
    go (ExRefF sym) = return $ ExRef sym
    go (ExVarF sym mv) =  ExVar sym <$> sequence mv
    go (ExTypF typ) = do
      env <- ask
      ntyp <- toDebruijn (Nothing :== typ) (fmap (\(a :@ _) -> a) env, [])
      ftyp <- fst <$> runReaderT (kinding (ntyp, 0)) env
      return $ ExTyp ftyp
    go (ExTupF lm) = ExTup <$> sequence lm
    go (ExRecF es) = ExRec <$> mapM sequence es
    go (ExAbsF (cookLambda -> lm)) = ExAbs <$> lm
    go (ExAppF mv1 mv2 mvn) = ExApp <$> mv1 <*> mv2 <*> sequence mvn
    go (ExLetF pat mv1 mv2) = ExLet <$> cookPattern pat <*> mv1 <*> mv2
    go (ExAnnoF (vp :@ typ)) = fmap ExAnno $ (:@) <$> vp <*> localKind typ

cookPattern :: forall c m. CookEnv c m => ParsePatternType -> m (ParsePattern (NormalType NormalKind (StandardRepType Symbol Label (Bound Symbol) Identity)))
cookPattern = cata go
  where
    go :: Base ParsePatternType (m (ParsePattern (NormalType NormalKind (StandardRepType Symbol Label (Bound Symbol) Identity))))
       -> m (ParsePattern (NormalType NormalKind (StandardRepType Symbol Label (Bound Symbol) Identity)))
    go PatWildF = return PatWild
    go PatUnitF = return PatUnit
    go (PatRefF name) = return $ PatRef name
    go (PatSymF name) = return $ PatSym name
    go (PatLitF lit) = return $ PatLit lit
    go (PatTupF lm) = PatTup <$> sequence lm
    go (PatRecF es) = PatRec <$> mapM sequence es
    go (PatSumF m1 lm) = PatSum <$> m1 <*> sequence lm
    go (PatViewF name mp) = PatView name <$> mp
    go (PatBindF name mp) = PatBind name <$> mp
    go (PatAnnoF (mp :@ typ)) = fmap PatAnno $ (:@) <$> mp <*> localKind typ

-- FIXME: handle the type declaration
cookLambda :: forall c m. CookEnv c m => ParseLambdaType -> m (ParseLambda (NormalType NormalKind (StandardRepType Symbol Label (Bound Symbol) Identity)))
cookLambda (Lambda _ b1 bs) = Lambda [] <$> cookBranch b1 <*> forM bs cookBranch
  where
    cookGpattern = cata \case
      PatternF pat -> Pattern <$> cookPattern pat
      PatSeqF rms -> PatSeq <$> sequence rms
      PatGrpF rms -> PatGrp <$> sequence rms
    cookBranch (pat, e) = (,) <$> cookGpattern pat <*> cookExpr e

nameRef, opRef :: String -> Type (Either Symbol b) cons bind inj rep
nameRef = TypRef . Left . Symbol
opRef = TypRef. Left . Op
localRef :: b -> Type (Either a b) cons bind inj rep
localRef = TypRef . Right

-- | convert well formed type into underlying type system
resolve :: a
resolve = error "not implemented"

-- | [p3, p2, p1], index starts from 1
lookupPrefix :: Integer -> Bounds Integer t -> Bound Integer t
lookupPrefix ix ls
  | length ls < fromInteger ix = error $ "no index of " <> show ix <> " found"
  | ix <= 0 = error "negative or zero index"
  | otherwise = ls !! (length ls - fromInteger ix)

-- | [p3, p2, p1], index starts from 1
updatePrefix :: Integer -> (Bound Integer t -> Bound Integer t)
             -> Bounds Integer t
             -> Bounds Integer t
updatePrefix ix u ls
  | length ls < fromInteger ix = error $ "exceeding index of " <> show ix
  | ix <= 0 = error "negative or zero index"
  | otherwise =
    case splitAt (length ls - fromInteger ix) ls of
      (h, v:hs) -> h <> (u v:hs)
      _ -> error "index overflow"

shift :: Integer -> Integer -> NormalType k r -> NormalType k r
shift reserve i t
  | reserve < 0 = error "invalid reserved number"
  | otherwise =
    let list = foldr (.) id $ S.Cons () . TypRef . Right <$> [1..reserve]
     in rewrite t $ list (() :! (reserve + i))

-- | remove rigid binding from prefixs
clean :: NormalType NormalKind r -> NormalType NormalKind r
clean (TypInj (t :@ k)) = TypInj (clean t :@ k)
clean (TypLit lit) =
  let handler = handleSum <$> prj @(Variant Label) lit
            <|> handleRec <$> prj @(Record Label) lit
            <|> handleTup <$> prj lit
   in case handler of
        Just v -> v
        Nothing -> TypLit lit
  where
    handleSum (Variant ts) = injTypeLit . Variant $ fmap (fmap clean) <$> ts
    handleRec (Record ts) = injTypeLit . Record $ fmap clean <$> ts
    handleTup (Tuple ts) = injTypeLit . Tuple $ clean <$> ts
-- FIXME:
-- clean qual@(TypAll _ _) =
--   let (prefixs, mono) = getMonoType qual
--    in uncurry (flip general) $ clean <$> reduce 0 (prefixs, mono)
--   where
--     reduce :: Integer
--            -> (Bounds Integer (NormalType NormalKind), NormalType NormalKind)
--            -> (Bounds Integer (NormalType NormalKind), NormalType NormalKind)
--     reduce reserve ((_ :~ t):ts, typeMono) =
--       let nts = (\(ix, ft) -> substitute ix (shift reserve ix t) <$> ft) <$> zip [1..] ts
--        in reduce reserve (nts, substitute (toInteger $ length ts + 1)
--                                           (shift reserve (toInteger $ length ts + 1) t) typeMono)
--     reduce reserve (pre@(_ :> _):ts, typeMono) =
--       let (pres, ntype) = reduce (reserve + 1) (ts, typeMono)
--        in (pre:pres, ntype)
--     reduce _ ([], typeMono) = ([], typeMono)
clean a = a

-- freeVars = cata \case
--   TypPht -> return []
--   TypRef (Right n) -> do
--     bound <- ask
--     if n <= bound then return [] else return [n]

-- | check that a is abstraction of b
-- TODO: finish abstraction checking
abstractof
  :: forall k m r
  . (MonadReader (Bounds Integer (NormalType k r)) m, MonadFail m, Eq k, Show k)
  => NormalType k r -> NormalType k r -> m ()
abstractof (TypInj (t1 :@ _)) (TypInj (t2 :@ _)) = abstractof t1 t2
abstractof t1 (TypInj (t2 :@ _)) = abstractof t1 t2
abstractof (TypInj (t1 :@ _)) t2 = abstractof t1 t2

-- TODO: finish the checking
abstractof (TypRef (Right _n1)) (TypRef (Right _n2)) = error "TODO: finish the checking"
abstractof _ _ = return ()

sortKey :: Ord key => [(key, val)] -> [(key, val)]
sortKey = sortBy (\a b -> compare (fst a) (fst b))

-- | index, new value
-- preserved substitution
substitute :: Integer -> NormalType k r -> NormalType k r -> NormalType k r
substitute ix t =
  let cells = S.Cons () . localRef <$> [1..ix-1]
   in flip rewrite $ foldr (\f b -> f b) (() :! 0) (cells <> [S.Cons () t])

-- | add binding to a monotype
general :: NormalType k r -> Bounds Integer (NormalType k r) -> NormalType k r
general mono qs = foldr ($) mono (injTypeBind @(Forall (Bound Integer)) . Forall <$> qs)

-- | split bounds
split :: Int -> Bounds Integer (NormalType k r) -> (Bounds Integer (NormalType k r), Bounds Integer (NormalType k r))
split v qs
  | length qs < v = error $ "Exceeded bounds: " <> show v
  | otherwise = splitAt (length qs - v) qs

-- | take a temporary bounds, and unify two types. generalize the unified type with the temporary bounds and return
-- modified global bounds.
scopUnify
  :: forall k m r
   . (MonadReader (Bounds Integer (NormalType k r)) m, MonadFail m, Eq k, Show k, Eq r, Show r)
  => Bounds Integer (NormalType k r) -> NormalType k r -> NormalType k r -> m (Bounds Integer (NormalType k r), NormalType k r)
scopUnify bounds t1 t2 = do
  (q1, t3) <- local (<> bounds) $ unify t1 t2
  return $ general t3 <$> split (length bounds) q1

-- | unify types with prefix
unify
  :: forall k m r
  . (MonadReader (Bounds Integer (NormalType k r)) m, MonadFail m, Eq k, Show k, Eq r, Show r)
  => NormalType k r -> NormalType k r -> m (Bounds Integer (NormalType k r), NormalType k r)
-- a temporary code for handling kind
unify (TypInj (t1 :@ _)) (TypInj (t2 :@ _)) = unify t1 t2
unify (TypInj (t1 :@ k)) t2 = fmap (TypInj . (:@ k)) <$> unify t1 t2
unify t1 (TypInj (t2 :@ k)) = fmap (TypInj . (:@ k)) <$> unify t1 t2

unify TypPht t = asks (, t)
unify t TypPht = asks (, t)

-- unify suports structural polymorphism
unify (TypLit e1) (TypLit e2) = do
  let handler = handleTup <$> prj e1 <*> prj e2
            <|> handleRec <$> prj e1 <*> prj e2
            <|> handleSum <$> prj e1 <*> prj e2
  case handler of
    Just m -> m
    Nothing -> fail "Unknown literal extension"
  where

    -- | unifier of tuple
    handleTup t1@(Tuple ts1) t2@(Tuple ts2) =
      if length ts1 /= length ts2
        then fail $ "Tuple doesn't match, left: " <> show t1 <> ", right: " <> show t2
        else do
          let tupUnify mr (l, r) = do
                (q1, ts) <- mr
                (q2, t) <- local (const q1) $ unify l r
                return (q2, ts <> [t])
          (q1, ts3) <- foldl tupUnify (asks (,[])) (zip ts1 ts2)
          return (q1, injTypeLit @Tuple $ Tuple ts3)

    -- | for handling unifying of record type
    handleRec t1@(Record ls1) t2@(Record ls2) = do
      if fromList (fst <$> ls1) /= fromList (fst <$> ls2)
      then fail $ "Record labels don't match, left: " <> show t1 <> ", right: " <> show t2
      else do
        let sort = sortBy \(a, _) (b, _) -> compare a b
            recUnify mr ((l1, lt1), (_l2, lt2)) = do
              (q, ts) <- mr
              (q2, t3) <- local (const q) $ unify lt1 lt2
              return (q2, ts <> [(l1, t3)])
        (q1, ts) <- foldl recUnify (asks (, [])) (sort ls1 `zip` sort ls2)
        return (q1, injTypeLit @(Record Label) $ Record ts)
    -- | for handling unifying of sum type
    handleSum t1@(Variant ls1) t2@(Variant ls2) = do
      if fromList (fst <$> ls1) /= fromList (fst <$> ls2)
      then fail $ "Variant labels don't match, left: " <> show t1 <> ", right: " <> show t2
      else do
        let sort = sortBy \(a, _) (b, _) -> compare a b
            sumUnify mr ((l1, Just lt1), (_l2, Just lt2)) = do
              (q, ts) <- mr
              (q2, t3) <- local (const q) $ unify lt1 lt2
              return (q2, ts <> [(l1, Just t3)])
            sumUnify mr ((l1, Nothing), (_, Nothing)) = do
              (q, ts) <- mr
              return (q, ts <> [(l1, Nothing)])
            sumUnify _ (l, r) = fail $ "Variant Field mismatch! left: " <> show l <> ", right: " <> show r
        (q1, ts) <- foldl sumUnify (asks (, [])) (sort ls1 `zip` sort ls2)
        return (q1, injTypeLit @(Variant Label) $ Variant ts)

unify (TypRef (Right n1)) (TypRef (Right n2))
  | n1 == n2 = asks (,localRef n1)
  | otherwise = do
    p1 <- asks $ lookupPrefix n1
    p2 <- asks $ lookupPrefix n2
    -- TODO, add abstraction check
    case (p1, p2) of
      (_ :> t1, _ :> t2) -> do
        -- flexible case
        (q1, t3) <- unify (shift 0 n1 t1) (shift 0 n2 t2)
        let q2 = updatePrefix (max n1 n2) (const $ 1 :> t3) q1
        return . (,localRef n1) $ updatePrefix (min n1 n2) (const $ 1 :~ localRef (abs $ n2 - n1)) q2
      (_ :~ t1, _ :~ t2) -> rigid (shift 0 n1 t1) (shift 0 n2 t2)
      (_ :> t1, _ :~ t2) -> rigid (shift 0 n1 t1) (shift 0 n2 t2)
      (_ :~ t1, _ :> t2) -> rigid (shift 0 n1 t1) (shift 0 n2 t2)
    where
      -- rigid :: NormalType k r -> NormalType k r -> m (Bounds Integer (NormalType k r))
      rigid t1 t2 = do
        (q1, t3) <- unify t1 t2
        let q2 = updatePrefix (max n1 n2) (const $ 1 :~ shift 0 (- (max n1 n2)) t3) q1
        return . (,localRef n1) $ updatePrefix (min n1 n2) (const $ 1 :~ localRef (abs $ n2 - n1)) q2

unify (TypRef (Right n1)) t2 =
  asks (lookupPrefix n1) >>= \case
    -- TODO: add abstraction check
    (_ :> t) -> do
      (q1, shift 0 (-n1) -> t1) <- unify (shift 0 n1 t) t2
      return (updatePrefix n1 (const $ 1 :~ t1) q1, localRef n1)
    (_ :~ t) -> do
      (q1, shift 0 (-n1) -> t1) <- unify (shift 0 n1 t) t2
      local (const q1) $ abstractof t1 t
      return (updatePrefix n1 (const $ 1 :~ t1) q1, localRef n1)

unify t2 t1@(TypRef (Right _)) = unify t1 t2

unify (TypCon h hs) (TypCon g gs)
  | length hs == length gs = do
    let through mq (h, g) = do
          (q1, ts) <- mq
          (q2, t) <- local (const q1) $ unify h g
          return (q2, ts <> [t])
    (q1, t1) <- unify h g
    (q2, ts) <- foldl through (return (q1, [])) $ zip hs gs
    return (q2, TypCon t1 ts)
  | otherwise = fail "Number of Type Constructor Parameter doesn't match"

-- | handle polymorphic type
unify t1@(TypLet bi1 ti1) t2@(TypLet bi2 ti2) = do
  case handleAll <|> handleAbs of
    Just m -> m
    Nothing -> fail "unknown quantifier binder"
  where
    handleAll =
      case (prj @(Forall (Bound Integer)) bi1, prj @(Forall (Bound Integer)) bi2) of
        (Just (Forall _), Just (Forall _)) -> Just do
          let (q1, mono1) = getMonoType t1
              (q2, mono2) = getMonoType t2
              t = shift 0 (toInteger $ length q2) mono1
          q0 <- ask
          (q3, t3) <- local (const $ q0 <> q1 <> q2) $ unify t (shift (toInteger $ length q2) (toInteger $ length q1) mono2)
          return $ (\f -> f t3) . foldr (.) id . fmap (injTypeBind . Forall) <$> splitAt (length q0) q3
        _ -> Nothing

    -- | for type operator, we may need HOU to handle it to make it compatible with type containment.
    -- but now we treat it simply and leave a comment.
    -- TODO: how to deal with unification problem of type operator
    -- we are going to strip off the bounds and take it as it is a quanitified type
    handleAbs =
      case prj @(Scope (Bound Integer)) bi1 of
        (Just (Scope _)) -> Just $ fail "can't prove euqality between type abstraction!"
        Nothing -> Nothing

unify t1@(TypLet bi1 ti1) t2 = do
  case handleAll of
    Just m -> m
    Nothing -> fail "unknown quantifier"
  where
    handleAll =
      case prj @(Forall (Bound Integer)) bi1 of
        Just (Forall _) -> Just do
          let (q1, mono1) = getMonoType t1
          q0 <- ask
          (q3, t3) <- local (const $ q0 <> q1) $ unify mono1 (shift 0 (toInteger $ length q1) t2)
          return $ (\f -> f t3) . foldr (.) id . fmap (injTypeBind @(Forall (Bound Integer)) . Forall) <$> splitAt (length q0) q3
        _ -> Nothing

unify t1 t2@(TypLet _ _) = unify t2 t1

-- | left with all simple one, compare and return. no type containment is needed.
-- for the type symbol, we compare the symbol name and don't compare the underlying
-- structure to preserve the property of phantom type.

unify a b =
  if a /= b
     then fail $ "Type mismatch!! left: " <> show a <> ", right: " <> show b
     else asks (,a)

-- | type environment, Γ. for inference.
-- \a -> \b -> \c -> ...
-- we have environment binding like:
-- c:tc,b:tb,a:ta
-- in a reverse order of introduction
type GammaEnv k r = [(Symbol, NormalType k r)]
-- | (unsolved term, solved term)
type GlobalEnv r = (GammaEnv NormalKind r, GammaEnv NormalKind r)
type TypingMonad r m =
  RWST (Bounds Integer (NormalType NormalKind r), GlobalEnv r) () (GammaEnv NormalKind r) m

data PatternError c a
  = DuplicateBinding a
  | BEContext c (PatternError c a)
  deriving (Eq, Ord, Functor)

$(makeBaseFunctor ''PatternError)

instance (Show a, Show c) => Show (PatternError c a) where
  show = cata \case
    DuplicateBindingF a -> "Duplicate binding of " <> show a
    BEContextF c r -> r <> "\n" <> "in " <> show c

runTyping :: (MonadFail m, Show r, Eq r)
          => (Maybe Symbol, ParseExpr (NormalType NormalKind r))
          -> GlobalEnv r
          -> GammaEnv NormalKind r
          -> m (ParseExpr (NormalType NormalKind r), GammaEnv NormalKind r, ())
runTyping e r = runRWST (typing e) ([], r)

-- | infer type of expreission
typing
  :: forall m r. (Monad m, MonadFail m, Show r, Eq r)
  => (Maybe Symbol, ParseExpr (NormalType NormalKind r))
  -> TypingMonad r m (ParseExpr (NormalType NormalKind r))
typing (_symbol'maybe, tree) = do
  (_prefix, val :@ _t) <- cata go tree
  return val
  where
    annotate val typ = ExAnno (val :@ typ) :@ typ

    go :: forall a. (a ~ (Bounds Integer (NormalType NormalKind r), NormalType NormalKind r :@ ParseExpr (NormalType NormalKind r)))
       => Base (ParseExpr (NormalType NormalKind r))
               (TypingMonad r m a)
       -> TypingMonad r m a
    go ExUnitF = asks $ (, ExUnit :@ (injTypeLit @Tuple $ Tuple [])) . fst
    go (ExLitF lit) =
      let typ = case lit of
                  LitInt _ -> nameRef "i8"
                  LitNumber _ -> nameRef "float"
                  LitString _ -> nameRef "str"
       in ask <&> (, ExLit lit `annotate` typ) . fst
    go (ExRefF name) = do
      res'maybe <- gets $ lookup name
      case res'maybe of
        Just t -> asks $ (,ExRef name `annotate` t) . fst
        Nothing -> fail $ "Undefined name of " <> show name
    go (ExVarF _ _) = error "Sorry, structural polymorphism is not supported now."
    go (ExSelF _) = error "Sorry, structural polymorphism is not supported now."
    go (ExTypF _typ) = error "Sorry, type application is not implemented now"
    go (ExTupF ems) = do
      let through mr mn = do
            (q1, ts, vs) <- mr
            (q2, v :@ t) <- local (first $ const q1) mn
            return (q2, ts <> [t], vs <> [v])
      (q, ts, vs) <- foldl through (asks $ (,[],[]) . fst) ems
      return (q, ExTup vs `annotate` (injTypeLit @Tuple . Tuple) ts)
    go (ExRecF ems) = do
      let through mr mn = do
            (q1, ts, vs) <- mr
            (label, (q2, v :@ t)) <- local (first $ const q1) $ sequence mn
            return (q2, ts <> [(Label case label of Symbol s -> s; Op s -> s,t)], vs <> [(label, v)])
      (q, ts, vs) <- foldl through (asks $ (,[],[]) . fst) ems
      return (q, ExRec vs `annotate` (injTypeLit @(Record Label) . Record) ts)
    go (ExAbsF _lambda) = error "lambda is not implemented yet"
    go (ExAppF m1 m2 mn) = do
      let reduction t1 t2 = do
            q <- asks fst
            (q2, getMonoType -> (prefix, _)) <-
              runReaderT (scopUnify [1 :> t1, 1 :> t2, 1 :> TypPht]
                                    (localRef 3)
                                    (TypCon (opRef "->") [localRef 2, localRef 1]))
                          q
            return (q2, general (localRef 1) prefix)
      (q1, v1 :@ t1) <- m1
      (q2, v2 :@ t2) <- local (first $ const q1) m2
      (q3, t3) <- local (first $ const q2) $ reduction t1 t2
      let through mr me = do
            (q, t, es) <- mr
            (qq, e :@ te) <- local (first $ const q) me
            (qqq, tt) <- local (first $ const qq) $ reduction t te
            return (qqq, tt, es <> [e])
      (q4, t4, vs) <- foldl through (return (q3, t3, [])) mn
      return (q4, ExApp v1 v2 vs `annotate` t4)
    go (ExLetF pat m1 m2) = do
      stat <- get
      rlen <- asks $ length . fst
      (npat :@ nt, (prefixs, stat1)) <- runStateT (patternTyping pat) mempty
      modify (<> stat1) -- add new bindings
      (q1, v1 :@ t1) <- local (first (<> prefixs)) m1
      (q3, _t3) <- runReaderT (unify nt t1) q1
      let (origin, bounds) = splitAt rlen q3
      -- generalize all binding introduced by pattern
      put (stat <> fmap (flip general bounds <$>) stat1)
      (q2, v2 :@ (clean -> t2)) <- local (first $ const origin) m2
      return (q2, ExLet npat v1 v2 `annotate` t2)
    go (ExAnnoF fmr) = do
      ((s1, e :@ t) :@ anno) <- sequence fmr
      (s2, _) <- runReaderT (unify (localRef 1) t) (s1 <> [1 :> anno])
      return (take (length s1) s2, ExAnno (e :@ t) `annotate` injTypeBind @(Forall (Bound Integer)) (Forall (1 :> anno)) (localRef 1))

-- collect names introduced by pattern, and do checking
bindCheck :: (MonadReader [Symbol] m, MonadError (PatternError (ParsePattern (NormalType k r)) Symbol) m)
          => ParsePattern (NormalType k r) -> m [Symbol]
bindCheck = para go
  where go :: (MonadReader [Symbol] m, MonadError (PatternError (ParsePattern (NormalType k r)) Symbol) m)
           => Base (ParsePattern (NormalType k r)) (ParsePattern (NormalType k r), m [Symbol])
           -> m [Symbol]
        go (PatRefF name) = do
          names <- ask
          if name `elem` names
             then throwError $ DuplicateBinding name
             else return [name]
        go (PatTupF ms) =
          catch (PatTup $ fst <$> ms) $
            foldM (\names (node, mr) -> catch node $ mappend names <$> local (<> names) mr)
            mempty ms
        go (PatRecF ms) = catch (PatRec $ fmap fst <$> ms)
          $ foldM (\names (snd -> (_, mr)) -> mappend names <$> local (<> names) mr) mempty ms
        go (PatSumF (n1, hm) ms) = catch n1 hm >>= \names ->
          catch (PatSum n1 $ fst <$> ms)
          $ foldM (\es (_, mr) -> mappend es <$> local (<> es) mr)
            names ms
        go (PatBindF name (node, mr)) = catch node $ (name:) <$> local (name:) mr
        go (PatViewF _ (node, mr)) = catch node mr
        go (PatAnnoF (hist :@ _)) = uncurry catch hist
        go _ = return mempty
        catch node m = catchError m (throwError . BEContext node)

-- collect introduced symbols and return its relevant type
patternTyping
  :: forall k m r
  . (MonadFail m, Eq k, Show k, MonadState (Bounds Integer (NormalType k r), GammaEnv k r) m, Show r, Eq r)
  => ParsePattern (NormalType k r)
  -> m (NormalType k r :@ ParsePattern (NormalType k r))
patternTyping pat = do
  vars <- runReaderT (runExceptT $ bindCheck pat) mempty >>= either (fail . show) return
  let env = fmap localRef <$> zip vars [1..]
      bounds = replicate (length vars) (1 :> TypPht)
  modify $ bimap (bounds <>) (env <>)
  cata go pat
  where
    annotate p t = p :@ t
    valOf (v :@ _) = v
    typOf (_ :@ t) = t
    newAnyType = do
      prefixs <- gets fst
      modify (first ((1 :> TypPht):)) -- append a new prefix
      return (localRef (toInteger $ length prefixs + 1))
    go PatWildF = annotate PatWild <$> newAnyType
    go PatUnitF = return (PatUnit :@ (injTypeLit @Tuple $ Tuple []))
    go (PatRefF name) = do
      env <- gets snd
      maybe (fail $ "can't find type for symbol " <> show name)
            (return . (PatRef name :@))
            $ lookup name env
      -- return (PatRef name :@ typ)
    go (PatTupF tups) = do
      tupPairs <- sequence tups
      return (PatTup (valOf <$> tupPairs) :@ (injTypeLit @Tuple . Tuple) (typOf <$> tupPairs))
    go (PatRecF recs) = do
      rs <- mapM sequence recs
      return (PatRec (fmap valOf <$> rs) :@ (injTypeLit @(Record Label) . Record) ((\(l, typOf -> t) -> (case l of Symbol s -> Label s; Op s -> Label s, t)) <$> rs))
    go (PatSymF _sym) = error "We need to lookup type definition to fetch full type for the variant label, not implemented yet"
    go (PatAnnoF (mp :@ t1)) = do
      p :@ t2 <- mp
      q1 <- gets fst
      (q2, t3) <- runReaderT (unify t2 t1) q1
      modify (first $ const q2)
      return (p :@ t3)
    go _ = error "sorry, feature is not implemented"

-- | TODO: add lambda type inference
lambdaTyping :: (MonadFail m, Eq k, Show k, MonadState (Bounds Integer (NormalType k r), GammaEnv k r) m, Show r, Eq r)
             => Lambda typ anno name -> m b
lambdaTyping (Lambda _ _branch _branchs) = do
  void $ gPatternTyping (error "not implemented")
  undefined
  where
    gPatternTyping (Pattern p) = patternTyping p
    gPatternTyping (PatGrp _ms) = do undefined
    gPatternTyping (PatSeq _ms) = do undefined

-- builtIn :: [KindedType TypAs k]
-- builtIn = TypAs . (\s -> TypSym s KindType False) <$>
--   [ "bool", "float", "double"
--   , "i8", "i16", "i32", "i128"
--   , "char", "short", "long"
--   , "u8", "u16", "u32", "u64", "byte"
--   , "str"
--   ] <*> [TypRep ()]

-- preTypeDef :: [VName :|-> SymbolString ExprType]
-- preTypeDef = (\(name, typ) -> fromString name :|-> Solved typ) <$> (prism ++ compounds)
--   where prism = fmap T.Lift <$>
--                 [ ("bool",  T.bool)
--                 , ("float", T.float)
--                 , ("double",T.double)
--                 , ("i8",    T.int8)
--                 , ("i16",   T.int16)
--                 , ("i32",   T.int32)
--                 , ("i128",  T.int128)
--                 , ("char",  T.char)
--                 , ("short", T.short)
--                 , ("long",  T.long)
--                 , ("u8",    T.uint8)
--                 , ("u16",   T.uint16)
--                 , ("u32",   T.uint32)
--                 , ("u64",   T.uint64)
--                 , ("byte",  T.byte)
--                 ]
--         compounds = [ ("str", T.Lift $ T.Ptr T.char Nothing)
--                     ]
