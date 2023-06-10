module Tlang.Graph.Extension.Type
  (
    -- ** Constant node
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

  , G (..)
  , P (..)  -- ^ permission

    -- ** Edge
  , Sub (..)
  , Bind (..)
  , Flag (..) -- ^ binding flag
  , Instance (..)
  , Unify (..)
  )
where

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

-- | arrow node, for ->
data NodeArr = NodeArr deriving (Show, Eq, Ord)

-- *** Constraint node

-- | `G` node, represents one level of generalization.
-- the integer indicates how many instances it now has.
newtype G = G Integer deriving (Show, Eq, Ord)

-- ** Edge definition

-- *** General edge

-- | structural edge, with number attached. orders matter.
newtype Sub = Sub Integer deriving (Show, Eq, Ord)
-- | binding edge, with number attached and also the origin name. orders and names matter.
data Bind name = Bind Flag Integer (Maybe name) deriving (Show, Eq, Ord)

-- | Used both for instance relation and permission
-- if `Lock` is taken as relation, then it is treated the same as `Rigid`
-- if `Explicit` is taken as permission, then it is treated the same as `Flexible`
data Flag
  = Explicit  -- ^ permission f, Flexible
  | Flexible  -- ^ permission f, Flexible
  | Rigid     -- ^ permission r, Rigid
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

-- | unify constraint edge, underictional edge
data Unify = Unify deriving (Show, Eq, Ord)

-- | instance constraint edge
data Instance = Instance Integer deriving (Show, Eq, Ord)
