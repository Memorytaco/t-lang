{- | We put definitions for graphic type constraints here.
--
-- Nodes are classified with 3 categories:
--
-- 1. type nodes: nodes which are wrapped by `T`
-- 2. constraint nodes: nodes like `G`. since we are constructing gMLF graphci constraint, constraint
--    nodes are not allowed appearing under type nodes.
-- 3. functional nodes: nodes like `Histo`. functional nodes are not "real nodes", they should not
--    affect structure of term graph and they should not affect meaning of constraints.
--
-- Edges act as both labels and structures. The most common edge type is `Sub` which acts
-- as most common, number labelled edge. When it is wrapped with `T`, it is structure edge.
-- And it indicates instances of a constraint `G` if it is wrapped with `C`. Since we have only
-- one type of constraint edge, we usually directly use `Sub`.
--
-- Wrappers are used to assign context relevant meaning to nodes and edges. We have
--
-- * `T` : to mean a node (edge) is part of a type
-- * `C` : to mean a node (edge) is part of constraint and it should not appear under a type node
--   > anything wrapped by `C` should not be linked by `Sub` in type graph
-- * `Pht` : to mean a node (edge) is only useful to a specific algorithm. We will never know its meaning
     unless we know what algorithm is using it.
-}
module Graph.Extension.GraphicType
  (

  -- ** Constant type node
    NodeSum (..)
  , NodeRec (..)
  , NodeHas (..)
  , NodeLit (..)

  , NodeApp (..)
  , NodeRef (..)
  , NodeRep (..)
  , NodeTup (..)
  , NodeBot (..)
  , NodeArr (..)
  , NodePht (..)

  -- ** Constraint node
  , G (..)

  -- ** Functional node
  , Histo (..)

  -- ** a wrapper to make difference among type nodes and constraint nodes
  -- or structure edges and constraint edges
  , T (..)
  , Pht (..)
  , C (..)
  , NDInfo
  , NDOrder (..)
  , NDOrderLink (..)

  -- ** Edge
  , E (..)
  , type (/.)
  , type (+.)
  , Sub (..)
  , O (..)
  , Binding (..)
  , Instance (..)
  , Unify (..)

  -- ** utility structures, usually used with nodes or edges

  -- *** permission
  , P (..)

  -- *** binding flag
  , Flag (..)
  )
where

import Data.Kind (Type)

-- ** Node definition

-- *** Constant node

-- | Variant node, tagged with field number
newtype NodeSum = NodeSum Integer deriving (Show, Eq, Ord)
-- | Record node, tagged with field number
newtype NodeRec = NodeRec Integer deriving (Show, Eq, Ord)
-- | Label node, the boolean indicates whether it has one sub node. `True` means it has one sub node.
data NodeHas name = NodeHas Bool name deriving (Show, Eq, Ord)
-- | Type literal node, parametrised by the actual lit representation
newtype NodeLit lit = NodeLit lit deriving (Show, Eq, Ord)

-- | Type application node
newtype NodeApp = NodeApp Integer deriving (Show, Eq, Ord)

-- | Type constructor node. It refers to global nominal type.
-- boolean indicates whether it is type alias. `True` means it is type alias.
data NodeRef name = NodeRef Bool name deriving (Show, Eq, Ord)

-- | primitive type node, inherit equality from its type parameter
newtype NodeRep r = NodeRep r deriving (Show, Eq, Ord)

-- | Tuple node, with integer indicating sub nodes.
newtype NodeTup = NodeTup Integer deriving (Show, Eq, Ord)

-- | bottom node, anonymous local type variable
data NodeBot = NodeBot deriving (Show, Eq, Ord)

-- | arrow node, for ->, has arity of 2
data NodeArr = NodeArr deriving (Show, Eq, Ord)

-- | a redundant annotation node, it means nothing
-- TODO: expand this node into annotation node to attach more information to nodes
data NodePht a = NodePht deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | TODO: to replace `Histo` nodes
-- This node family is meant to define a family of functional nodes.
data family NDInfo x

-- | a `Histo` is meant to remember every merged node
newtype Histo a = Histo [a]
  deriving (Functor, Foldable, Traversable)
  deriving (Show, Eq, Ord) via [a]

-- | a special node used to track constraint dependency
data NDOrder a = NDOrder
  deriving (Functor, Foldable, Traversable)
  deriving (Show, Eq, Ord)

-- | a special node used to track constraint dependency
data NDOrderLink = NDOrderLink
  deriving (Show, Eq, Ord)

-- *** Graph decorator
--
-- used to decorate things and gives them different meanings
-- or itself means something.

-- | `G` node, represents one level of generalization.
-- the integer indicates how many instances it now has.
newtype G a = G Integer deriving (Show, Eq, Ord, Functor)

-- | `T` node, wrap concrete type nodes
newtype T t a = T t
  deriving (Show, Functor, Foldable, Traversable)
  deriving (Eq, Ord) via t

-- | `C` node, wrap things and behaves as constraint
newtype C c a = C c
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via c

-- | `Pht` phantom node, it attach additional information to nodes.
--
-- Any nodes wrapped by this should not interfere operation on "normal"
-- nodes, like constraint nodes or type nodes.
newtype Pht c a = Pht c
  deriving (Eq, Ord, Functor, Foldable, Traversable)
  deriving Show via c

-- ** Edge definition

-- | a boring edge with no information attached
--
-- this edge is unique and expresses connection only
data O = O deriving (Show, Eq, Ord)

-- *** General edge

-- define parameter
data a /. b
data a +. b

-- | Edge family
--
-- __Experimental__
data family E (proxy :: k)

-- | indexed edge
newtype instance E "structure"
  = E Integer
  deriving (Show, Eq, Ord)

-- | indexed binding edge
data instance E ("binding" /. (name :: Type))
  = B Flag Integer (Maybe name)
  deriving (Show, Eq, Ord)

-- | unification edge, with no direction
data instance E "unify"
  = Unify' deriving (Show, Eq, Ord)

data instance E (a +. b)
  = El (E a)
  | Er (E b)
instance (Show (E a), Show (E b)) => Show (E (a +. b)) where
  show (El e) = show e
  show (Er e) = show e
deriving instance (Eq (E a), Eq (E b)) => Eq (E (a +. b))
deriving instance (Ord (E a), Ord (E b)) => Ord (E (a +. b))

-- | structural edge, with number attached. orders matter.
newtype Sub = Sub Integer deriving (Show, Eq, Ord)

-- | binding edge, with number attached and also the origin name. orders and names matter.
data Binding name = Binding Flag Integer (Maybe name) deriving (Show, Eq, Ord)

-- | Used both for instance relation and permission
-- if `Lock` is taken as relation, then it is treated the same as `Rigid`
-- if `Explicit` is taken as permission, then it is treated the same as `Flexible`
data Flag
  = Explicit  -- ^ permission f, Flexible, user provided
  | Flexible  -- ^ permission f, Flexible, automatic generated
  | Rigid     -- ^ permission r, Rigid   , automatic generated
  deriving (Show, Eq, Ord)

-- | Permission for each node
data P
  = PM -- ^ Monomorphic node
  | PI -- ^ Inert node, limited reversible operation
  | PG -- ^ Green node, every operation
  | PO -- ^ Orange node, only operation for representation
  | PR -- ^ Red node, no operation
  deriving (Show, Eq, Ord)

-- *** Constraint edge

-- | unify constraint edge, undirectional edge
data Unify = Unify deriving (Show, Eq, Ord)

-- | instance constraint edge
newtype Instance = Instance Integer deriving (Show, Eq, Ord)
