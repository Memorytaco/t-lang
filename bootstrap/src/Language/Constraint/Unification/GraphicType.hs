{- | * Graph unification module

    implement core unification algorithm
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Constraint.Unification.GraphicType
  (

  -- ** Core environment
    UnifyConstraint

  -- ** unification error data and sub data
  , GraphUnifyError (..)
  , GraphProperty (..)

  -- ** basic error handler
  , failMsg
  , failProp
  , unmatch

  -- ** Core structure
  , Unifier (..)
  , runUnifier

  , Case (..)
  , foldCase

  -- ** algorithm instance
  , case1, case2
  , case10
  , case20, case21, case25
  , case30
  , case40, case41
  , case50, case51, case52
  , case60

  -- ** algorithm hook
  , hook1
  )
where

import Language.Core (Name (..))
import Graph.Core
import Graph.Extension.GraphicType
import Language.Generic ((:<:), prj, Recursion2 (..))

import Control.Monad (when, unless, forM, foldM)
import Data.Functor ((<&>), ($>))
import Capability.Error (HasThrow, HasCatch, throw, catch)
import Capability.State (HasState, get, gets, modify)
import Data.Bifunctor (bimap)
import Data.List (nub, groupBy)
import Data.Set (toList)
import Data.Function (fix)

-- import Data.Kind (Constraint, Type)

-------------------------------------
-- ** Common environment
-------------------------------------

type UnifyConstraint m ns es info =
  ( HasThrow "failure" (GraphUnifyError (Hole ns info)) m
  , HasState "graph" (CoreG ns es info) m
  , T NodeBot :<: ns, Histo :<: ns
  , HasOrderGraph ns es info
  , T Sub :<: es
  , Pht O :<: es
  )

------------------------------------------
-- ** Core structures
------------------------------------------

-- | a small reuseable code snippet
newtype Unifier m ns info
  = Unifier (Recursion2 m (Hole ns info) (Hole ns info) (Hole ns info))

-- | a sequence matcher of unifier, can be converted to Unifier
newtype Case m ns info = Case [Unifier m ns info]

-- | a wrapper to extend functionality of unifier
type Decorator m ns info = Unifier m ns info -> Unifier m ns info

----------------------------------------------
-- ** companion utility
----------------------------------------------

-- | to merge one layer of unifiers into a single one.
--
--    CaseT [u1, u2, u3 ...] :=
--      case (a, b) of
--        if match u1, then u1 a b
--        if match u2, then u2 a b
--        ...
foldCase
  :: ( HasCatch "failure" (GraphUnifyError (Hole ns info)) m
     , Eq info, Eq (ns (Hole ns info))
     )
  => Case m ns info -> Unifier m ns info
foldCase (Case us) =
  let foldT (Unifier (Recursion2 f)) (Unifier (Recursion2 g)) = Unifier . Recursion2 $ \r a b ->
        catch @"failure" (f r a b) $ \case
          e@(FailWithUnmatch ea eb) ->
            -- we deal with only one layer, since `r` is a recursive call
            if ea == a && eb == b || ea == b || eb == a
               -- if it is at the same level, we pass control to next unifier
               then g r a b
               -- if not, it means there is a recursive call and all unifiers can't handle these two nodes.
               -- thus we return the error.
               else throw @"failure" e
          e -> throw @"failure" e
   in foldr foldT (Unifier $ Recursion2 \_ a b -> unmatch a b) us

-- | unwrap a unifier to a normal function
runUnifier :: Unifier m ns info -> Hole ns info -> Hole ns info -> m (Hole ns info)
runUnifier (Unifier (Recursion2 f)) = fix f

---------------------------------------
-- ** Hooks, to augment existed Unifier
---------------------------------------

-- | unification hook pre: G
--
-- add guard to detect existence of `G` node, constraint nodes can not be unified
hook1 :: (G :<: ns, HasThrow "failure" (GraphUnifyError (Hole ns info)) m) => Decorator m ns info
hook1 (Unifier (Recursion2 f)) = Unifier . Recursion2 $ \r a b -> do
  let tru _ _ = True
  when (isHole @G a tru || isHole @G b tru) $ failMsg "unexpected G node"
  f r a b

------------------------------------------
-- ** Unifiers
------------------------------------------

-- | unification: NodeBot left
case1 :: (UnifyConstraint m ns es info, T (Binding Name) :<: es)
      => Unifier m ns info
case1 = Unifier $ Recursion2 \_unify -> match unmatch \(a, T NodeBot, _) b ->
  -- merge node, keep `b`
  a ==> b >> rebind @Name b $> b

-- | unification: NodeBot right
case2 :: (UnifyConstraint m ns es info, T (Binding Name) :<: es)
      => Unifier m ns info
case2 = Unifier $ Recursion2 \_unify a -> match (unmatch a) \(b, T NodeBot, _) ->
  -- merge node, keep `a`
  b ==> a >> rebind @Name a $> a


-- | unification: NodeTup
case10
  :: (UnifyConstraint m ns es info, T NodeTup :<: ns, T (Binding Name) :<: es)
  => Unifier m ns info
case10 = Unifier $ Recursion2 \unify ->
  match unmatch \(a, T (NodeTup s), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeTup t), _ib) -> do
    -- arity of tuple should match
    when (t /= s) $ failProp $ NodeDoesn'tMatch a b
    -- merge node, keep `a`
    b ==> a >> rebind @Name a
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
    sequel unify pairs $> a

-- | unification: NodeRec
case20
  :: (UnifyConstraint m ns es info, T NodeRec :<: ns, T (Binding Name) :<: es)
  => Unifier m ns info
case20 = Unifier $ Recursion2 \unify ->
  match unmatch \(a, T (NodeRec an), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeRec bn), _ib) -> do
    -- structure node should have same number of subnodes
    when (an /= bn) $ failProp $ NodeDoesn'tMatch a b
    -- merge node, keep `a`
    b ==> a >> rebind @Name a
    -- unify subnodes
    children <- getsGraph $ lFrom @(T Sub) (== a)
    if toInteger (length children) == an + bn then do
      pairs <- forM (groupBy (\(fst -> x) (fst -> y) -> x == y) children) \case
        [snd -> x, snd -> y] -> return (x, y)
        _ -> failMsg "NodeRec nodes don't have same number of children"
      sequel unify pairs $> a
    else failMsg "Unexpected internal encoding error, record structure node has wrong number of subnodes"

-- | unification: NodeSum
case21
  :: ( UnifyConstraint m ns es info
     , T NodeSum :<: ns
     , T (Binding Name) :<: es
     )
  => Unifier m ns info
case21 = Unifier $ Recursion2 \unify ->
  match unmatch \(a, T (NodeSum an), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeSum bn), _ib) -> do
    -- structure node should have same number of subnodes
    when (an /= bn) $ failProp $ NodeDoesn'tMatch a b
    -- merge node, keep `a`, and rebind binding edge
    b ==> a >> rebind @Name a
    -- unify subnodes
    children <- getsGraph $ lFrom @(T Sub) (== a)
    if toInteger (length children) == an + bn then do
      pairs <- forM (groupBy (\(fst -> x) (fst -> y) -> x == y) children) \case
        [snd -> x, snd -> y] -> return (x, y)
        _ -> failMsg "NodeSum nodes don't have same number of children"
      sequel unify pairs $> a
    else failMsg "Unexpected internal encoding error, variant structure node has wrong number of subnodes"

-- | unification: NodeHas
case25
  :: forall label m ns es info.
     ( UnifyConstraint m ns es info
     , T (NodeHas label) :<: ns
     , T (Binding Name) :<: es
     , Eq label
     )
  => Unifier m ns info
case25 = Unifier $ Recursion2 \unify ->
  match @(T (NodeHas label)) unmatch \(a, T (NodeHas af al), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeHas bf bl), _ib) -> do
    -- a label must match exactly, otherwise unification fails
    unless (af == bf && al == bl) $ failProp $ NodeDoesn'tMatch a b
    -- merge node, keep `a`
    b ==> a >> rebind @Name a
    if af && bf then do
      children <- getsGraph $ lFrom @(T Sub) (== a)
      case snd <$> children of
        [x, y] -> unify x y $> a
        _ -> failMsg "Unexpected internal encoding error, label node should have exact one child but it doesn't"
    else return a

-- | unification: NodeApp
case30
  :: ( UnifyConstraint m ns es info
     , T NodeApp :<: ns
     , T (Binding Name) :<: es
     )
  => Unifier m ns info
case30 = Unifier $ Recursion2 \unify ->
  match unmatch \(a, T (NodeApp an), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeApp bn), _ib) -> do
    -- structure node should have same number of subnodes
    when (an /= bn) $ failProp $ NodeDoesn'tMatch a b
    -- merge node, keep `a`
    b ==> a >> rebind @Name a
    -- unify subnodes
    children <- getsGraph $ lFrom @(T Sub) (== a)
    if toInteger (length children) == (an + bn) then do
      pairs <- forM (groupBy (\(fst -> x) (fst -> y) -> x == y) children) \case
        [snd -> x, snd -> y] -> return (x, y)
        _ -> failMsg "NodeApp nodes don't have same number of children"
      sequel unify pairs $> a
    else failMsg "Unexpected internal encoding error, type application node has wrong number of subnodes"
    -- TODO: check name node and deal with type alias, it is complex
    -- we now assume no type alias is involved

-- | unification: NodeLit
--
-- unification for literal type node
case40
  :: forall lit ns es info m.
     ( UnifyConstraint m ns es info
     , T (NodeLit lit) :<: ns
     , T (Binding Name) :<: es
     , Eq lit
     )
  => Unifier m ns info
case40 = Unifier $ Recursion2 \_unify ->
  match @(T (NodeLit lit)) unmatch \(a, T (NodeLit lita), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeLit litb), _ib) -> do
    when (lita /= litb) $ failProp $ NodeDoesn'tMatch a b
    b ==> a >> rebind @Name a >> return a

-- | unification: NodeRep
--
-- unification for representation type node
case41
  :: forall rep ns es info m.
     ( UnifyConstraint m ns es info
     , T (NodeRep rep) :<: ns
     , T (Binding Name) :<: es
     , Eq rep
     )
  => Unifier m ns info
case41 = Unifier $ Recursion2 \_unify ->
  match @(T (NodeRep rep)) unmatch \(a, T (NodeRep repa), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeRep repb), _ib) -> do
    -- representation has been shipped with equivalence relation
    -- so we simply compare and rebind
    when (repa /= repb) $ failProp $ NodeDoesn'tMatch a b
    b ==> a >> rebind @Name a >> return a

-- | unification: NodePht both
case50
  :: ( NodePht :<: ns, UnifyConstraint m ns es info
     , T (Binding Name) :<: es
     )
  => Unifier m ns info
case50 = Unifier $ Recursion2 \unify ->
  match @NodePht unmatch \(a, NodePht, _) ->
  match @NodePht (unmatch a) \(b, NodePht, _) -> do
    -- we merge these two phantom nodes first
    b ==> a >> rebind @Name a
    -- then merge two subnodes
    children <- getsGraph $ lFrom @(T Sub) (== a)
    case snd <$> children of
      [x, y] -> unify x y $> a
      _ -> failMsg "NodePht node should have exactly one subnode but it is not"

-- | unification: NodePht left
case51
  :: (NodePht :<: ns, UnifyConstraint m ns es info)
  => Unifier m ns info
case51 = Unifier $ Recursion2 \unify ->
  match @NodePht unmatch \(a, NodePht, _) b -> do
    children <- getsGraph $ lFrom @(T Sub) (== a)
    case children of
      [snd -> x] -> unify x b
      _ -> failMsg "NodePht node should have exactly one subnode but it is not"

-- | unification: NodePht right
case52
  :: (NodePht :<: ns, UnifyConstraint m ns es info)
  => Unifier m ns info
case52 = Unifier $ Recursion2 \unify a ->
  match @NodePht (unmatch a) \(b, NodePht, _) -> do
    children <- getsGraph $ lFrom @(T Sub) (== b)
    case children of
      [snd -> x] -> unify x a
      _ -> failMsg "NodePht node should have exactly one subnode but it is not"

-- | unification: NodeRef
case60
  :: forall name ns es info m.
     ( UnifyConstraint m ns es info
     , T (NodeRef name) :<: ns
     , T (Binding Name) :<: es
     , Eq name
     )
  => Unifier m ns info
case60 = Unifier $ Recursion2 \_unify ->
  match @(T (NodeRef name)) unmatch \(a, T (NodeRef aliasa namea), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeRef aliasb nameb), _ib) -> do
    -- when name nodes both are not alias, then use syntax directed type equality
    when (not (aliasa || aliasb) && namea /= nameb) $ failProp $ NodeDoesn'tMatch a b
    -- TODO: check type alias, alias node should be defined in `NodeApp`
    -- we now assume no type alias is involved
    -- merge node, keep `a`
    b ==> a >> rebind @Name a >> return a

----------------------------------------
-- ** Environment operator
----------------------------------------

getsGraph :: HasState "graph" (CoreG ns es info) m => (CoreG ns es info -> a) -> m a
getsGraph = gets @"graph"
{-# INLINE getsGraph #-}

modifyGraph :: HasState "graph" (CoreG ns es info) m => (CoreG ns es info -> CoreG ns es info) -> m ()
modifyGraph = modify @"graph"
{-# INLINE modifyGraph #-}

----------------------------
-- ** Unification Error
----------------------------

-- | Error or exception
data GraphUnifyError node
  = FailWithProperty (GraphProperty node)   -- ^ some graph constraint property doesn't hold
  | FailWithMessage String                  -- ^ fail with arbitrary message, unrecoverable error
  | FailWithUnmatch node node               -- ^ don't know how to deal with these two nodes
  deriving (Show, Eq)

data GraphProperty node
  = NodeWrongWithBinder node          -- ^ every node should have exactly one binder
  | NodeDoesn'tMatch node node        -- ^ two nodes have different category and thus no equality
  | NodeNoLeastCommonBinder node node -- ^ no least common binder for these two nodes
  deriving (Show, Eq)

-------------------
-- ** Error handler
-------------------

-- | fail with an message
failMsg :: HasThrow "failure" (GraphUnifyError (Hole ns info)) m => String -> m a
failMsg = throw @"failure" . FailWithMessage

-- | fail with unsatisfied property
failProp :: HasThrow "failure" (GraphUnifyError (Hole ns info)) m => GraphProperty (Hole ns info) -> m a
failProp = throw @"failure" . FailWithProperty

-- | fail with unmatched nodes, it behaves like a failure of pattern match and is recoverable
unmatch :: HasThrow "failure" (GraphUnifyError node) m => node -> node -> m a
unmatch a b = throw @"failure" $ FailWithUnmatch a b

-- | a handler to help with node matching
--
-- usage:
--
-- 1. to match left branch
--      a -> a -> ?
--      match unmatch \(left, tag, info) right -> {do anything}
-- 2. to match right branch
--      a -> a -> ?
--      \left -> match (unmatch left) \(right, tag, info) -> {do anything}
-- 3. to match both branch
--      a -> a -> ?
--      match unmatch \(left, tag, info) -> match id (right, tag2, info2) -> {do anything}
match :: forall tag ns info b. tag :<: ns
      => (Hole ns info -> b)
      -> ((Hole ns info, tag (Hole ns info), info) -> b) -> Hole ns info -> b
match f g n@(Hole tag info) =
  case prj @tag tag of
    Just v -> g (n, v, info)
    Nothing -> f n
{-# INLINE match #-}

----------------------------------------
-- ** Useful utility
----------------------------------------

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
binderInfo :: (Ord (es (Link es)), T (Binding name) :<: es, Ord info, Ord (ns (Hole ns info)))
           => Hole ns info -> CoreG ns es info -> [(Flag, Integer, Maybe name, Hole ns info)]
binderInfo node g = lFrom (== node) g >>= \(T (Binding flag i name), binder) -> return (flag, i, name, binder)

-- | rebind binding edge for node n, this should happen after `==>`.
rebind :: forall name ns es info m
        . ( HasState "graph" (CoreG ns es info) m
          , HasThrow "failure" (GraphUnifyError (Hole ns info)) m
          , HasOrderGraph ns es info
          , T (Binding name) :<: es, T NodeBot :<: ns, T Sub :<: es
          , Eq name
          , Histo :<: ns, Pht O :<: es
          )
       => Hole ns info -> m ()
rebind n = do
  binders :: [(Flag, Integer, Maybe name, Hole ns info)] <- getsGraph $ binderInfo n
  let bn1 = binders <&> \(_, _, _ , b) -> b
      flag = maximum $ binders <&> \(f, _, _, _) -> f
  bn2 <- partial
  (b, bs) <- case nub $ bn1 <> bn2 of
               a:as -> return (a, as)
               [] -> failProp $ NodeWrongWithBinder n
               {- Important!!! for every node, there should exist a binder. -}
  bn <- foldM lcb b bs
  i <- maximum <$> forM binders \(f, i, name, v) ->
    modifyGraph (filterLink (`isLink` \(T (Binding f' i' name')) -> f == f' && i == i' && name == name') n v) $> i
  modifyGraph $ overlay (n -<< T (Binding flag i (Nothing :: Maybe name)) >>- bn)
  where
    -- | return direct ancestors of node n, if it is partially grafted node.
    -- if this is used during unification, '==>' should be used first.
    partial = do
      candidates <- get @"graph" <&> transpose <&> \g -> toList $ reachable (`isLink` \(T (Sub _)) -> True) g n
      -- a partially grafted node only requires that a bottom node exists among
      -- its merged ancestors.
      --
      -- we iterate structurally reachable nodes of "n" and query linked `Histo` nodes
      -- to get all merged nodes. Finally we test whether there exists bottom
      -- node and return result.
      --
      -- results are merged using "or".
      bools <- forM candidates \node -> do
        merged <- getsGraph (lFrom @(Pht O) (== node))
          <&> fmap snd -- get nodes
          <&> (>>= maybeHole (const []) (\_ (Histo as) _ -> as)) -- unroll Histo nodes
        return . or $ maybeHole @(T NodeBot) (const False) (\_ _ _ -> True) <$> merged
      if or bools
         then getsGraph $ nub . fmap snd . lTo @(T Sub) (== n)
         else return []
    -- | least common binder of two nodes
    lcb n1 n2 = do
      ns1 <- getsGraph \g -> dfs (`isLink` (\(T (Binding _ _ (_ :: Maybe name))) -> True)) g n1
      ns2 <- getsGraph \g -> dfs (`isLink` (\(T (Binding _ _ (_ :: Maybe name))) -> True)) g n2
      case zip ns1 ns2 >>= \(a, b) -> if a == b then pure a else [] of
        [] -> failProp $ NodeNoLeastCommonBinder n1 n2
        ls -> return $ last ls

-- | merge operation for nodes
--
-- It replaces `from` node with `to` node, and return the second one.
--
-- It also track merged nodes using Histo node.
--
-- `Histo` provides a service to keep tracking every merged node.
--
-- `Histo` is not a concrete node, and it should not appear in either constraint nodes
-- or type nodes.
(==>) :: (HasState "graph" (CoreG ns es info) m, Eq info, Eq (ns (Hole ns info)), Pht O :<: es, Histo :<: ns, Ord (es (Link es)))
      => Hole ns info -> Hole ns info -> m (Hole ns info)
(==>) from to@(Hole _ info) = do

  -- substitutes `from` node to `to` node
  modifyGraph (fmap \node -> if node == from then to else node)

  -- for every two merged nodes, we add a phantom link between result
  -- node and the Histo node.
  --
  -- "info" is randomly picked using resulting node's info.
  let pht = hole (Histo [from, to]) info
  modifyGraph $ overlay (to -<< Pht O >>- pht)
  return to
