{- | * Graph unification module

    implement core unification algorithm
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Tlang.Unification.Type
  (

  -- ** Core environment
    (:~~:) (..)
  , GraphConstraint

  , Uno (..)
  , Histo (..)
  , (~=~)
  )
where

import Tlang.Graph.Core
import Tlang.Graph.Extension.Type
import Tlang.Generic ((:<:) (..), (:+:) (..))

import Control.Monad (when, unless, forM, foldM)
import Data.Functor ((<&>), ($>))
import Capability.Error (HasThrow)
import Capability.State (HasState)
import Data.Bifunctor (bimap)
import Data.List (nub, groupBy, intercalate)
import Data.Set (toList)

import Data.Kind (Constraint, Type)

-- ** a frame where to put definition

type GraphConstraint :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> (Type -> Type) -> Constraint
type family GraphConstraint entity node edge info m

class node :~~: info | node -> info where
  gunify :: ( HasState "graph" (CoreG ns es info) m
            , HasThrow "failure" (GraphUnifyError (Hole ns info)) m
            , GraphConstraint node ns es info m
            , Eq (ns (Hole ns info)), Ord (es (Link es))
            , node :<: ns)
         => (Hole ns info -> Hole ns info -> m (Hole ns info))
         -> (node (Hole ns info), info) -> Hole ns info -> m (Hole ns info)

type instance GraphConstraint (a :+: b) n e i m = (GraphConstraint a n e i m, GraphConstraint b n e i m, a :<: n, b :<: n)
instance (f :~~: a, g :~~: a) => (f :+: g) :~~: a where
  gunify f (Inr v, a) = gunify f (v, a)
  gunify f (Inl v, a) = gunify f (v, a)

-- ** algorithm instance

-- | Wraper for extension
newtype Uno c a = Uno c
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via c

-- | a `Histo` is meant to remember every merged node
data Histo a = Histo a [a]
  deriving (Functor, Foldable, Traversable)
instance Ord a => Ord (Histo a) where
  compare (Histo a _) (Histo b _) = compare a b
instance Eq a => Eq (Histo a) where
  Histo a _ == Histo b _ = a == b
instance Show a => Show (Histo a) where
  show (Histo a as) = "{" <> show a <> ": " <> intercalate "," (show <$> as) <> "}"

true :: a -> b -> Bool
true _ _ = True

type instance GraphConstraint Histo n e i m = ()
instance Histo :~~: Int where
  -- `Histo` provides a service to keep track of every merged node.
  -- every other nodes need to handle `Histo` too to be compatible which is unfortunate inconvenience.
  -- TOOD: refine codes
  gunify unify (Histo a h1, _) b@(Hole tag _) = case prj @Histo tag of
    -- we here to handle two `Histo` cases, and merge history nodes
    Just (Histo c h2) -> unify a c >>= \d@(Hole tag2 n) -> do
      case prj @Histo tag2 of
        Just (Histo e h3) -> return $ hole n (Histo e . nub $ h1 <> h2 <> h3)
        Nothing -> return $ hole n (Histo d . nub $ a:c: h1 <> h2)
    -- if the other one is not `Histo` node, we simply call `unify` to handle the two nodes, and
    -- check out returened node whether to be again `Histo` node.
    -- if returned node is `Histo`, merge returned history and add new item to history.
    Nothing -> unify a b >>= \c@(Hole tag2 n) ->
      case prj @Histo tag2 of
        Just (Histo d h2) -> return $ hole n (Histo d . nub $ h2 <> h1)
        Nothing -> return $ hole n (Histo c . nub $ b:h1)

type instance GraphConstraint (T NodeBot) n e i m
  = ( G :<: n, Histo :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i))
    )
-- | unification for bottom node
instance T NodeBot :~~: Int where
  gunify unify (T NodeBot, n1) a@(Hole tag n) = do
    -- some exceptions that a bottom node can't be unified with
    when (isHole @G a true) $ failMsg "unexpected G node"
    -- handle `Histo` node explicitly
    case prj tag of
      Just (Histo _ _) -> unify a $ hole n1 (T NodeBot)
      Nothing -> do
        -- merge node, keep `a`
        let v = hole n (Histo a [hole n1 $ T NodeBot, a])
        hole n1 (T NodeBot) ==> a >> a ==> v >> rebind @String v $> v

type instance GraphConstraint (T NodeTup) n e i m
  = ( G :<: n, Histo :<: n, T NodeBot :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i))
    )
-- | unification for tuple
instance T NodeTup :~~: Int where
  gunify unify (T (NodeTup s), n1) a@(Hole tag _) =
    if isHole @(T NodeBot) a true || isHole @Histo a true then
      unify a $ hole n1 (T $ NodeTup s)
    else case prj @(T NodeTup) tag of
      Just (T (NodeTup t)) -> do
        -- arity of tuple should match
        when (t /= s) $ failProp $ NodeDoesn'tMatch (hole n1 (T (NodeTup s))) a
        -- merge node, keep `a`
        hole n1 (T $ NodeTup s) ==> a >> rebind @String a
        -- unify subnodes, now these subnodes share same sub edge number
        {-
                  (ng)      or    (ng)
            |1  |2  |1  |2      |1  |1  |2  |2
            n1  n2  g1  g2      n1  g1  n1  g2

            since sub edge is tagged with a number, and result is sorted, it will be
            the second case. we group it by equivalence edge.
        -}
        children <- getsGraph $ lFrom @(T Sub) (== a)
        pairs <- forM (groupBy (\(fst -> x) (fst -> y) -> x == y) children) \case
          [snd -> x, snd -> y] -> return (x, y)
          _ -> failMsg "Tuple nodes don't have same number of children"
        sequel unify pairs >> return a
      Nothing -> failProp $ NodeDoesn'tMatch (hole n1 (T (NodeTup s))) a


type instance GraphConstraint (T (NodeHas label)) n e i m
  = ( G :<: n, Histo :<: n, T NodeBot :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i)), Eq label
    )
-- | unification for label
instance T (NodeHas label) :~~: Int where
  gunify unify (T (NodeHas b1 label1), n1) a@(Hole tag _) = case prj @(T (NodeHas label)) tag of
    Just (T (NodeHas b2 label2)) -> do
      -- a label must match exactly, otherwise unification fails
      unless (b1 == b2 && label1 == label2) $ failProp $ NodeDoesn'tMatch (hole n1 (T (NodeHas b1 label1))) a
      -- merge node, keep `a`
      hole n1 (T $ NodeHas b1 label1) ==> a >> rebind @String a
      -- b1 == `True` means it has one child, and that should match too
      if b1 then do
        children <- getsGraph $ lFrom @(T Sub) (== a)
        case snd <$> children of
          [x, y] -> unify x y $> a
          _ -> failMsg "Unexpected internal encoding error, label node should have exact one child but it doesn't"
      else return a
    Nothing -> failProp $ NodeDoesn'tMatch (hole n1 (T (NodeHas b1 label1))) a


type instance GraphConstraint (T NodeSum) n e i m
  = ( G :<: n, Histo :<: n, T NodeBot :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i))
    )
-- | unification for variant node
instance T NodeSum :~~: Int where
  gunify unify (T (NodeSum s1), n1) a@(Hole tag _) =
    if isHole @(T NodeBot) a true || isHole @Histo a true then
      unify a (hole n1 . T $ NodeSum s1)
    else case prj @(T NodeSum) tag of
      Just (T (NodeSum s2)) -> do
        -- structure node should have same number of subnodes
        when (s1 /= s2) $ failProp $ NodeDoesn'tMatch (hole n1 (T $ NodeSum s1)) a
        -- merge node, keep `a`, and rebind binding edge
        hole n1 (T $ NodeSum s1) ==> a >> rebind @String a
        -- unify subnodes
        children <- getsGraph $ lFrom @(T Sub) (== a)
        if toInteger (length children) == s1 + s2 then do
          pairs <- forM (groupBy (\(fst -> x) (fst -> y) -> x == y) children) \case
            [snd -> x, snd -> y] -> return (x, y)
            _ -> failMsg "Variant nodes don't have same number of children"
          sequel unify pairs $> a
        else failMsg "Unexpected internal encoding error, variant structure node has wrong number of subnodes"
      Nothing -> failProp $ NodeDoesn'tMatch (hole n1 . T $ NodeSum s1) a


type instance GraphConstraint (T NodeRec) n e i m
  = ( G :<: n, Histo :<: n, T NodeBot :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i))
    )
-- | unification for record node
instance T NodeRec :~~: Int where
  gunify unify (T (NodeRec s1), n1) a@(Hole tag _) =
    if isHole @(T NodeBot) a true || isHole @Histo a true then
      unify a (hole n1 . T $ NodeRec s1)
    else case prj @(T NodeRec) tag of
      Just (T (NodeRec s2)) -> do
        -- structure node should have same number of subnodes
        when (s1 /= s2) $ failProp $ NodeDoesn'tMatch (hole n1 (T $ NodeRec s1)) a
        -- merge node, keep `a`
        hole n1 (T $ NodeRec s1) ==> a >> rebind @String a
        -- unify subnodes
        children <- getsGraph $ lFrom @(T Sub) (== a)
        if toInteger (length children) == s1 + s2 then do
          pairs <- forM (groupBy (\(fst -> x) (fst -> y) -> x == y) children) \case
            [snd -> x, snd -> y] -> return (x, y)
            _ -> failMsg "Variant nodes don't have same number of children"
          sequel unify pairs $> a
        else failMsg "Unexpected internal encoding error, record structure node has wrong number of subnodes"
      Nothing -> failProp $ NodeDoesn'tMatch (hole n1 . T $ NodeRec s1) a

type instance GraphConstraint (T NodeApp) n e i m
  = ( G :<: n, Histo :<: n, T NodeBot :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i))
    )
-- | unification for variant node
instance T NodeApp :~~: Int where
  gunify unify (T (NodeApp s1), n1) a@(Hole tag _) =
    if isHole @(T NodeBot) a true || isHole @Histo a true then
      unify a (hole n1 . T $ NodeApp s1)
    else case prj @(T NodeApp) tag of
      Just (T (NodeApp s2)) -> do
        -- structure node should have same number of subnodes
        when (s1 /= s2) $ failProp $ NodeDoesn'tMatch (hole n1 (T $ NodeApp s1)) a
        -- merge node, keep `a`
        hole n1 (T $ NodeApp s1) ==> a >> rebind @String a
        -- unify subnodes
        children <- getsGraph $ lFrom @(T Sub) (== a)
        if toInteger (length children) == s1 + s2 then do
          pairs <- forM (groupBy (\(fst -> x) (fst -> y) -> x == y) children) \case
            [snd -> x, snd -> y] -> return (x, y)
            _ -> failMsg "Variant nodes don't have same number of children"
          sequel unify pairs $> a
        else failMsg "Unexpected internal encoding error, type application node has wrong number of subnodes"
        -- TODO: check name node and deal with type alias, it is complex
        -- we now assume no type alias is involved
      Nothing -> failProp $ NodeDoesn'tMatch (hole n1 . T $ NodeApp s1) a

type instance GraphConstraint (T (NodeRep a)) n e i m
  = ( G :<: n, Histo :<: n, T NodeBot :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i))
    )
-- | unification for representation type node
instance Eq a => T (NodeRep a) :~~: Int where
  gunify unify (T (NodeRep r1), n1) a@(Hole tag _) =
    if isHole @(T NodeBot) a true || isHole @Histo a true then
      unify a (hole n1 . T $ NodeRep r1)
    else case prj @(T (NodeRep a)) tag of
      Just (T (NodeRep r2)) -> do
        -- representation has shipped equivalence relation
        when (r1 /= r2) $ failProp $ NodeDoesn'tMatch (hole n1 (T $ NodeRep r1)) a
        hole n1 (T $ NodeRep r1) ==> a >> rebind @String a >> return a
      Nothing -> failProp $ NodeDoesn'tMatch (hole n1 (T $ NodeRep r1)) a

type instance GraphConstraint (T (NodeRef name)) n e i m
  = ( G :<: n, Histo :<: n, T NodeBot :<: n
    , T (Bind String) :<: e, T Sub :<: e
    , Show (n (Hole n i)), Ord (n (Hole n i))
    , Eq name
    )
-- | unification for variant node
instance T (NodeRef name) :~~: Int where
  gunify unify (T (NodeRef b1 s1), n1) a@(Hole tag _) =
    if isHole @(T NodeBot) a true || isHole @Histo a true then
      unify a (hole n1 . T $ NodeRef b1 s1)
    else case prj @(T (NodeRef name)) tag of
      Just (T (NodeRef b2 s2)) -> do
        -- when name nodes are both no alias, then use syntax directed type equality
        when (not (b1 || b2) && s1 /= s2) $ failProp $ NodeDoesn'tMatch (hole n1 (T $ NodeRef b1 s1)) a
        -- TODO: check type alias, alias node should be defined in `NodeApp`
        -- we now assume no type alias is involved
        -- merge node, keep `a`
        hole n1 (T $ NodeRef b1 s1) ==> a >> rebind @String a >> return a
      Nothing -> failProp $ NodeDoesn'tMatch (hole n1 . T $ NodeRef b1 s1) a

-- ** useful operator

-- | unification operator
(~=~) :: ( GraphConstraint ns ns es info m, ns :~~: info
         , HasState "graph" (CoreG ns es info) m
         , HasThrow "failure" (GraphUnifyError (Hole ns info)) m
         , Ord (es (Link es)), ns :<: ns, Eq (ns (Hole ns info)))
      => Hole ns info -> Hole ns info -> m (Hole ns info)
(~=~) (Hole node info) = gunify (~=~) (node, info)

-- | unify a list of nodes, and it permits duplication of nodes, it doesn't keep order between nodes
sequel :: (Monad m, Eq a) => (a -> a -> m a) -> [(a, a)] -> m [a]
sequel _ [] = return []
sequel unify ((a,b): xs) = do
  val <- unify a b
  let gone = if val == a then b else a
      subst i = if i == gone then val else i
  vals <- sequel unify $ bimap subst subst <$> xs
  return $ val : vals

-- | get node's binder and flag
binderInfo :: (Eq info, Eq (ns (Hole ns info)), Ord (es (Link es)), T (Bind name) :<: es)
           => Hole ns info -> CoreG ns es info -> [(Flag, Integer, Maybe name, Hole ns info)]
binderInfo node g = lFrom (== node) g >>= \(T (Bind flag i name), binder) -> return (flag, i, name, binder)

-- | rebind binding edge for node n, this should happen after `==>`.
rebind :: forall name ns es info m
        . ( HasState "graph" (CoreG ns es info) m
          , HasThrow "failure" (GraphUnifyError (Hole ns info)) m
          , Show info, Show (ns (Hole ns info))
          , Ord info, Ord (ns (Hole ns info)), Ord (es (Link es))
          , Eq name
          , T (Bind name) :<: es, T NodeBot :<: ns, T Sub :<: es, Histo :<: ns
          )
       => Hole ns info -> m ()
rebind n = do
  binders :: [(Flag, Integer, Maybe name, Hole ns info)] <- getsGraph $ binderInfo n
  let bn1 = binders >>= \(_, _, _ , b) -> pure b
      flag = maximum $ binders >>= \(f, _, _, _) -> pure f
  bn2 <- partial
  (b, bs) <- case nub $ bn1 <> bn2 of
               a:as -> return (a, as)
               [] -> failMsg ""
  bn <- foldM lcb b bs
  i <- maximum <$> forM binders \(f, i, name, v) ->
    modifyGraph (filterLink (`isLink` \(T (Bind f' i' name')) -> f == f' && i == i' && name == name') n v) $> i
  modifyGraph $ overlay (n -<< T (Bind flag i (Nothing :: Maybe name)) >>- bn)
  where
    -- | return direct ancestors of node n, if it is partially grafted node.
    -- if this is used during unification, '==>' should be used first.
    partial = do
      ns <- getGraph <&> transpose <&> \g -> toList $ reachable (`isLink` \(T (Sub _)) -> True) g n
      if or $ flip isHole (\_ (Histo _ as) -> or $ flip isHole (\_ (T NodeBot) -> True) <$> as) <$> ns
         then getsGraph $ nub . fmap snd . lTo @(T Sub) (== n)
         else return []
    -- | least common binder of two nodes
    lcb n1 n2 = do
      ns1 <- getsGraph \g -> dfs (`isLink` (\(T (Bind _ _ (_ :: Maybe name))) -> True)) g n1
      ns2 <- getsGraph \g -> dfs (`isLink` (\(T (Bind _ _ (_ :: Maybe name))) -> True)) g n2
      case zip ns1 ns2 >>= \(a, b) -> if a == b then pure a else [] of
        [] -> failMsg $ "No least common binder for " <> show n1 <> ", " <> show n2
        ls -> return $ last ls

-- | replace first node with second node, and return the second one
(==>) :: (HasState "graph" (CoreG ns es info) m, Eq info, Eq (ns (Hole ns info)))
      => Hole ns info -> Hole ns info -> m (Hole ns info)
(==>) from to = modifyGraph (fmap \node -> if node == from then to else node) >> return to
