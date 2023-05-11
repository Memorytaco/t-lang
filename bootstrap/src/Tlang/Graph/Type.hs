module Tlang.Graph.Type
  (
    GEdge (..)
  , GConstraint (..)
  , GFlag (..)
  , Perm (..)
  , GNode (..)
  , GNodeLabel (..)
  , NodeArity (..)
  )
where

import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Label (StrLabel))
import Data.Text.Lazy (pack)
import Tlang.Graph.Core

import Tlang.AST (Symbol (..), Variance (..))

-- | label for edge
data GEdge a
  = GSub Int
  -- ^ structural edge, with number attached. orders matter.
  | GBind GFlag Int (Maybe a)
  -- ^ binding edge, with number attached and also the origin name. orders and names matter.
  | GOperate GConstraint
  -- ^ operation edge that indicates one action. **now it serves as constraint edge only.**
  deriving (Show, Eq, Ord)

-- | Constraint edge
data GConstraint
  = CUnify        -- ^ unify constraint edge
  | CInstance Int -- ^ instance constraint edge
  deriving (Show, Eq, Ord)

-- | Used both for instance relation and permission
-- if `Lock` is taken as relation, then it is treated the same as `Rigid`
-- if `Explicit` is taken as permission, then it is treated the same as `Flexible`
data GFlag
  = Rigid     -- ^ permission r, Rigid
  | Explicit  -- ^ permission f, Flexible
  | Flexible  -- ^ permission f, Flexible
  deriving (Show, Eq, Ord)

-- | permission related to `GFlag`
data Perm
  = PRed    -- ^ mixed with rigid and flexible, lowest permission
  | POrange -- ^ first flexible and then rigid, middle permission
  | PGreen  -- ^ all time be the flexible, largest permission
  | PInert  -- ^ nodes have no bounded sub node
  | PMono   -- ^ momorphic nodes, have no polymorphic nodes
  deriving (Show, Eq, Ord)

-- | label for node
data GNode typ
  = GNode     -- ^ scheme node, the **G** node, with sort *scheme*
  | GType typ -- ^ type node, with sort *type*
  deriving (Show, Eq, Ord, Functor)

-- | actual label for Type, doesn't support predicate constraint
data GNodeLabel lit label rep name
  = NodeUni -- unit type node, arity 0
  | NodeBot -- special bottom node, arity 0
  | NodeRep rep -- representation node, arity 0
  | NodeLit lit -- literal type node, arity 0
  | NodeRef name  -- type ref node, used to hold concrete type, arity 0
  | NodeSum -- has multiple labels, and each label may have one type
  | NodeRec -- has multiple lables, and each label must have one type
  | NodeTup -- has as many sub nodes (graphs) as user want, preserve the order
  | NodeHas label -- type label node, depends on its parent, can have one or no sub graph, arity 1
  | NodeApp -- application node, minimal arity 2
  | NodeAbs -- abstraction node, arity 2
  -- | NodeCons name NodeArity
  deriving (Show, Eq)

-- | mark arity info of constructor
newtype NodeArity = NodeArity [Variance] deriving (Show, Eq)

instance (Labellable name, Show label) => Labellable (GNodeLabel lit label rep name) where
  toLabelValue NodeUni = StrLabel "()"
  toLabelValue NodeBot = StrLabel "⊥"
  toLabelValue (NodeRep _) = StrLabel "&"
  toLabelValue (NodeLit _) = StrLabel "$"
  toLabelValue (NodeRef name) = toLabelValue name
  -- toLabelValue (NodeCons name _) = toLabelValue name
  toLabelValue NodeSum = StrLabel "<>"
  toLabelValue NodeRec = StrLabel "{}"
  toLabelValue NodeTup = StrLabel "*"
  toLabelValue (NodeHas label) = StrLabel (pack $ "$" <> show label)
  toLabelValue NodeAbs = StrLabel "λ"
  toLabelValue NodeApp = StrLabel "@"

instance Labellable typ => Labellable (GNode typ) where
  toLabelValue GNode = StrLabel ".G."
  toLabelValue (GType typ) = toLabelValue typ

instance Show a => Labellable (GEdge a) where
  toLabelValue (GSub i) = toLabelValue i
  toLabelValue (GBind _perm ix (Just name)) = StrLabel . pack $ show ix <> ":" <> show name
  toLabelValue (GBind _perm ix Nothing)  = StrLabel . pack $ show ix
  toLabelValue (GOperate CUnify) = StrLabel "**Unify**"
  toLabelValue (GOperate (CInstance i)) = StrLabel . pack $ show i <> " |>"
