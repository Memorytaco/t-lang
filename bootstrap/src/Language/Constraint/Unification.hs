{- | * Graph unification module

  implementation of core unification algorithm.

  TODO: Detect infinite type recursion, see unit test cases for example
  TODO: Add instance relation correction check

  ** special cases

  1. EQ, if two nodes are the same, does nothing.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Constraint.Unification
  (

  -- ** Core environment
    UnifyConstraint

  -- ** unification error
  , GraphUnifyError (..)
  , GraphProperty (..)
  , HasGraphUnifyError

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
  , case30, case31
  , case40, case41
  , case50, case51, case52
  , case60

  -- ** algorithm group
  , caseNodePht

  -- ** algorithm hook
  , hook1

  -- ** Internal
  , rebind
  , (==>)
  , sequel
  )
where

import Graph.Algorithm (lms)
import Graph.Core
import Graph.Data (order)
import Graph.Extension.GraphicType
import Language.Core (Name (..))
import Language.Generic (Recursion2 (..), prj, (:<:))
import Language.Setting (HasGraph, HasGraphShow, getsGraph, modifyGraph)

import Capability.Error (HasCatch, HasThrow, catch, throw)
import Control.Lens ((^..), _1, _3)
import Control.Monad (foldM, forM, unless, when, (>=>))
import Data.Bifunctor (bimap)
import Data.Function (fix)
import Data.Functor (($>), (<&>))
import Language.Constraint.Relation.Phantom
import Language.Generic.Subsume ((:>+:))

-------------------------------------
-- ** Common environment
-------------------------------------

type UnifyConstraint m ns es info =
  ( HasGraphUnifyError (Hole ns info) m
  , HasGraphShow ns es info
  , HasGraph ns es info m
  , T NodeBot :<: ns, R [] :<: ns
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
  :: ( HasGraphUnifyError (Hole ns info) m
     , Eq info, Eq (ns (Hole ns info))
     )
  => Case m ns info -> Unifier m ns info
foldCase (Case us) =
  let foldT (Unifier (Recursion2 f)) (Unifier (Recursion2 g)) = Unifier . Recursion2 $ \r a b ->
        catchUnifyErr (f r a b) $ \case
          e@(FailWithUnmatch ea eb) ->
            -- we deal with only one layer, since `r` is a recursive call
            if ea == a && eb == b || ea == b || eb == a
               -- if it is at the same level, we pass control to next unifier
               then g r a b
               -- if not, it means there is a recursive call and all unifiers can't handle these two nodes.
               -- thus we return the error.
               else throwUnifyErr e
          e -> throwUnifyErr e
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
hook1 :: (G :<: ns, HasGraphUnifyError (Hole ns info) m) => Decorator m ns info
hook1 (Unifier (Recursion2 f)) = Unifier . Recursion2 $ \r a b -> do
  when (isHoleOf @G a || isHoleOf @G b) $ failMsg "unexpected G node"
  f r a b

------------------------------------------
-- ** Groups, used to bind things together
------------------------------------------

-- | group unification: NodePht
--
-- Phantom node is a special type structure node, it needs careful
-- setup to not mess things up.
caseNodePht :: ( NodePht :<: ns, UnifyConstraint m ns es info, T (Binding Name) :<: es) => Unifier m ns info
caseNodePht = foldCase $ Case [case50, case51, case52]

------------------------------------------
-- ** Unifiers
------------------------------------------

-- | unification: NodeBot left
case1 :: (UnifyConstraint m ns es info, T (Binding Name) :<: es)
      => Unifier m ns info
case1 = Unifier $ Recursion2 \_unify -> match unmatch \(a, T NodeBot, _) b ->
  a |-> b

-- | unification: NodeBot right
case2 :: (UnifyConstraint m ns es info, T (Binding Name) :<: es)
      => Unifier m ns info
case2 = Unifier $ Recursion2 \_unify a -> match (unmatch a) \(b, T NodeBot, _) ->
  b |-> a


-- | unification: NodeTup
case10
  :: (UnifyConstraint m ns es info, T NodeTup :<: ns, T (Binding Name) :<: es)
  => Unifier m ns info
case10 = Unifier $ Recursion2 \unify ->
  match unmatch \(a, T (NodeTup s), _ia) ->
  match (unmatch a) \(b, T (NodeTup t), _ib) -> do
    -- arity of tuple should match
    when (t /= s) $ failProp $ NodeDoesn'tMatch a b
    as <- getsGraph (lFrom @(T Sub) a)
    bs <- getsGraph (lFrom @(T Sub) b)
    ab <- if length as == length bs
              then return (zip (snd <$> as) (snd <$> bs))
              else failMsg "Tuple nodes don't have same number of children"
    -- merge node, keep `a`
    b |-> a >>= (sequel unify ab $>)

-- | unification: NodeRec
case20
  :: (UnifyConstraint m ns es info, T NodeRec :<: ns, T (Binding Name) :<: es)
  => Unifier m ns info
case20 = Unifier $ Recursion2 \unify ->
  match unmatch \(a, T (NodeRec an), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeRec bn), _ib) -> do
    -- structure node should have same number of subnodes
    when (an /= bn) $ failProp $ NodeDoesn'tMatch a b
    as <- getsGraph $ lFrom @(T Sub) a
    bs <- getsGraph $ lFrom @(T Sub) b
    ab <- if length as == length bs
             then return (zip (snd <$> as) (snd <$> bs))
             else failMsg "NodeRec nodes don't have same number of children"
    b |-> a >>= (sequel unify ab $>)

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
    as <- getsGraph $ lFrom @(T Sub) a
    bs <- getsGraph $ lFrom @(T Sub) b
    ab <- if length as == length bs
             then return (zip (snd <$> as) (snd <$> bs))
             else failMsg "NodeSum nodes don't have same number of children"
    -- merge node, keep `a`, and rebind binding edge
    b |-> a >>= (sequel unify ab $>)

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
  match (unmatch a) \(b, T (NodeHas bf bl), _ib) -> do
    -- a label must match exactly, otherwise unification fails
    unless (af == bf && al == bl) $ failProp $ NodeDoesn'tMatch a b
    as <- getsGraph $ lFrom @(T Sub) a
    bs <- getsGraph $ lFrom @(T Sub) b
    ab <- if length as == 1 && length bs == 1
             then return (zip (snd <$> as) (snd <$> bs))
             else failMsg "Unexpected internal encoding error, label node should have exact one child but it doesn't"
    -- merge node, keep `a`
    b |-> a >>= (sequel unify ab $>)

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
    as <- getsGraph $ lFrom @(T Sub) a
    bs <- getsGraph $ lFrom @(T Sub) b
    ab <- if length as == length bs
             then return (zip (snd <$> as) (snd <$> bs))
             else failMsg "NodeApp nodes don't have same number of children"
    -- merge node, keep `a`
    -- TODO: check name node and deal with type alias, it is complex
    -- we now assume no type alias is involved
    b |-> a >>= (sequel unify ab $>)

-- | unification: NodeArr
case31
  :: ( UnifyConstraint m ns es info
     , T NodeArr :<: ns
     , T (Binding Name) :<: es
     )
  => Unifier m ns info
case31 = Unifier $ Recursion2 \_unify ->
  match unmatch \(a, T NodeArr, _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T NodeArr, _ib) -> do
    b |-> a

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
    b |-> a

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
    b |-> a

-- | unification: NodePht both
case50
  :: ( NodePht :<: ns, UnifyConstraint m ns es info
     , T (Binding Name) :<: es
     )
  => Unifier m ns info
case50 = Unifier $ Recursion2 \unify ->
  match @NodePht unmatch \(a, NodePht, _) ->
  match @NodePht (unmatch a) \(b, NodePht, _) -> do
    as <- getsGraph $ lFrom @(T Sub) a
    bs <- getsGraph $ lFrom @(T Sub) b
    ab <- if length as == 1 && length bs == 1
             then return (zip (snd <$> as) (snd <$> bs))
             else failMsg "NodePht node should have exactly one subnode but it is not"
    -- we merge these two phantom nodes first
    b |-> a >>= (sequel unify ab $>) >>= phtGuard

-- | unification: NodePht left
case51
  :: (NodePht :<: ns, T (Binding Name) :<: es, UnifyConstraint m ns es info)
  => Unifier m ns info
case51 = Unifier $ Recursion2 \unify ->
  match @NodePht unmatch \(pht, NodePht, _) b -> do
    phtShrink pht >>= unify b >>= phtRaise pht

-- | unification: NodePht right
case52
  :: (NodePht :<: ns, T (Binding Name) :<: es, UnifyConstraint m ns es info)
  => Unifier m ns info
case52 = Unifier $ Recursion2 \unify a ->
  match @NodePht (unmatch a) \(pht, NodePht, _) -> do
    phtShrink pht >>= unify a >>= phtRaise pht

-- | unification: NodeRef
-- TODO: complete node application
case60
  :: forall name ns es info m.
     ( UnifyConstraint m ns es info
     , T (NodeRef name) :<: ns
     , T (Binding Name) :<: es
     , Eq name
     )
  => Unifier m ns info
case60 = Unifier $ Recursion2 \_unify ->
  match @(T (NodeRef name)) unmatch \(a, T (NodeRef aritya namea), _ia) ->
  match (failProp . NodeDoesn'tMatch a) \(b, T (NodeRef arityb nameb), _ib) -> do
    -- when their arity doesn't match or their symbol doesn't match,
    -- it is a failure.
    when (aritya /= arityb || namea /= nameb) $ failProp $ NodeDoesn'tMatch a b
    -- TODO: check type alias, alias node should be defined in `NodeApp`
    -- we now assume no type alias is involved
    -- merge node, keep `a`
    b |-> a

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
  = -- | every node should have exactly one binder
    NodeWrongWithBinder node
  | -- | two nodes have different category and thus no equality
    NodeDoesn'tMatch node node
  | -- | no least common binder for these two nodes
    NodeNoLeastCommonBinder node [node] node [node]
  deriving (Show, Eq)

-- | setting for unification
type HasGraphUnifyError node m =
  ( HasThrow GraphUnifyError (GraphUnifyError node) m
  , HasCatch GraphUnifyError (GraphUnifyError node) m
  )

-------------------
-- ** Error handler
-------------------

-- | fail with an message
failMsg :: HasGraphUnifyError (Hole ns info) m => String -> m a
failMsg = throw @GraphUnifyError . FailWithMessage

-- | fail with unsatisfied property
failProp :: HasGraphUnifyError (Hole ns info) m => GraphProperty (Hole ns info) -> m a
failProp = throw @GraphUnifyError . FailWithProperty

-- | fail with unmatched nodes, it behaves like a failure of pattern match and is recoverable
unmatch :: HasGraphUnifyError node m => node -> node -> m a
unmatch a b = throw @GraphUnifyError $ FailWithUnmatch a b

-- | catch unification error
catchUnifyErr :: HasGraphUnifyError node m => m a -> (GraphUnifyError node -> m a) -> m a
catchUnifyErr = catch @GraphUnifyError

-- | throw unification error
throwUnifyErr :: HasGraphUnifyError node m => GraphUnifyError node -> m a
throwUnifyErr = throw @GraphUnifyError

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
match fls tru n@(Hole tag info) =
  case prj @tag tag of
    Just v -> tru (n, v, info)
    Nothing -> fls n
{-# INLINE match #-}

----------------------------------------
-- ** Useful utility
----------------------------------------

-- | unify a list of nodes, and it permits duplication of nodes, it doesn't keep order between nodes.
--
-- If there are two identical nodes, it does nothing.
--
-- * when EQ, do nothing
sequel :: (Monad m, Eq a) => (a -> a -> m a) -> [(a, a)] -> m [a]
sequel _ [] = return []
sequel unify ((a,b): xs)
  | a == b = sequel unify xs
  | otherwise = do
    val <- unify a b
    let gone = if val == a then b else a
        subst i = if i == gone then val else i
    vals <- sequel unify $ bimap subst subst <$> xs
    return $ val : vals

-- | Merge Relation, from |-> to, replace `from` with `to` and delete `from` and keep `to`.
--
-- if `from` and `to` are the same, it does nothing.
--
-- * when EQ, do nothing
(|->) ::
  ( HasGraphUnifyError (Hole ns info) m, HasGraph ns es info m, HasOrderGraph ns es info
  , es :>+: '[T (Binding Name), T Sub, Pht O]
  , ns :>+: '[R [], T NodeBot]
  )
  => Hole ns info -> Hole ns info -> m (Hole ns info)
a |-> b
  | a == b = return a -- unify and rebind an equivalent is not necessary
  | otherwise = a ==> b >>= rebind @Name

-- | rebind binding edge for node n, this should happen after `==>`.
rebind :: forall name ns es info m
        . ( HasGraph ns es info m
          , HasGraphUnifyError (Hole ns info) m
          , HasOrderGraph ns es info
          , T (Binding name) :<: es, T NodeBot :<: ns, T Sub :<: es
          , Ord name
          , R [] :<: ns, Pht O :<: es
          )
       => Hole ns info -> m (Hole ns info)
rebind n = do
  binders <- getsGraph (binderOf n)
  let bn1 = binders ^.. traverse . _3
      flag = maximum $ binders ^.. traverse . _1
  -- fetch direct ancestors if this is a partial grafted node
  bn2 <- partial
  (b, bs) <- case order (bn1 <> bn2) of
               a:as -> return (a, as)
               -- Important!!! for every node, there should exist exactly one binder and, in
               -- this case, returned list should never be empty
               [] -> failProp $ NodeWrongWithBinder n
  -- final binder of node `n`
  bn <- foldM lcb b bs
  -- remove all binding edge out from `n`
  modifyGraph . einduce $ tryLink @(T (Binding name)) (\e a' b' -> Just (e, a', b'))
     \e@(T (Binding {})) a' b' -> if a' == n then Nothing else Just (link e, a', b')
  -- add the binding edge from root to its computed binder
  modifyGraph $ overlay (n -<< T (Binding flag (Nothing :: Maybe name)) >>- bn)
  gr <- getsGraph id
  return gr $> n
  where
    -- | return direct ancestors of node n, if it is partially grafted node.
    -- if this is used during unification, '==>' should be used first.
    partial = do
      candidates <- getsGraph transpose <&> \g -> reachable (isLinkOf @(T Sub)) g n
      -- a partially grafted node only requires that a bottom node exists among
      -- its merged ancestors.
      --
      -- we iterate structurally reachable nodes of "n" and query linked `Histo` nodes
      -- to get all merged nodes. Finally we test whether there exists bottom
      -- node and return result.
      --
      -- results are merged using "or".
      bools <- forM candidates \node -> do
        merged <- do
          -- Mn should not include 'n'
          if node == n then return []
          else getsGraph (lFrom @(Pht O) node >=> tryHole (const []) (\_ (R as) _ -> as) . snd)
        -- check each merged node whether it is a bottom node
        return (any (isHoleOf @(T NodeBot)) merged)
      if or bools
         then getsGraph do order . fmap fst . lTo @(T Sub) n
         else return []
    -- | least common binder of two nodes
    lcb n1 n2 = do
      ns1 <- getsGraph \g -> dfs (isLinkOf @(T (Binding name))) g n1
      ns2 <- getsGraph \g -> dfs (isLinkOf @(T (Binding name))) g n2
      case lms ns1 ns2 of
        [] -> failProp $ NodeNoLeastCommonBinder n1 ns1 n2 ns2
        a:_ -> return a

    -- | get node's binder and flag
    binderOf :: Hole ns info -> CoreG ns es info -> [(Flag, Maybe name, Hole ns info)]
    binderOf node g = lFrom node g >>= \(T (Binding flag name), binder) -> return (flag, name, binder)

-- | merge operation for nodes
--
-- It merges `from` and `to` node in @from ==> to@ and return `to` node.
--
-- It also track merged nodes using @I []@, which is named as "Track" node.
--
-- Track node provides a service to keep tracking every merged node in this algorithm.
--
-- Track node is not a concrete node as indicated by prefix @I@, and it should
-- not be structure connected to either constraint nodes or type nodes.
(==>) :: (HasGraph ns es info m, HasEqGraph ns es info, Pht O :<: es, R [] :<: ns)
      => Hole ns info -> Hole ns info -> m (Hole ns info)
(==>) from to@(Hole _ info) = do

  -- map `from` node to `to` node
  modifyGraph (fmap \node -> if node == from then to else node)

  -- for every two merged nodes, we add a phantom link between result
  -- node and the track node.
  --
  -- "info" is randomly picked using resulting node's info, we use
  -- the second info here.
  let pht = hole (R [from, to]) info
  modifyGraph (overlay (to -<< Pht O >>- pht)) $> to
