module Tlang.AST.Expr
  ( Expr (..)
  , ExprF (..)
  , Pattern (..)
  , PatternF (..)
  , GPattern (..)
  , GPatternF (..)
  , Lambda (..)
  , Literal (..)
  , Field (..)
  , (:@) (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)

import Tlang.AST.Type (Bound (..))

-- | type annotation with full power of the type system
data typ :@ term = term :@ typ deriving (Show, Eq, Functor, Traversable, Foldable)

$(deriveBifunctor ''(:@) )

-- | builtin literal value
data Literal
  = LitInt Integer
  | LitNumber Double
  | LitString String
  deriving (Show, Eq)

-- | Expression, parametric with binary operator and also one info tag.
data Expr typ anno name
  = ExUnit        -- ^ unit value, **()**
  | ExLit Literal -- ^ literal value and its type
  | ExRef name    -- ^ variable or term info reference
  | ExSel name    -- ^ `.selector`, field projector
  | ExTyp typ     -- ^ unlift type to term level, enable explicit type application
  | ExTup [Expr typ anno name]                -- ^ tuple value
  | ExRec [(name, Expr typ anno name)]        -- ^ record constructor
  | ExVar name (Maybe (Expr typ anno name))   -- ^ `` `Variant `` for polymorphic variant, variant constructor
  | ExAbs (Lambda typ anno name)        -- ^ We allow full power of lambda in expression
  | ExApp (Expr typ anno name)          -- ^ Lambda beta reduction, aka. function application
          (Expr typ anno name)
          [Expr typ anno name]
  | ExLet (Pattern anno name)           -- ^ local variable binding
      (Expr typ anno name) (Expr typ anno name)
  | ExAnno (anno (Expr typ anno name))  -- ^ type annotation for expression

-- | instantiation
data Inst name typ
  = InstOne       -- identity
  | InstTyp typ   -- bottom
  | InstNew name  -- abstract
  | InstInside (Inst name typ)
  | InstUnder name (Inst name typ)
  | InstElim
  | InstIntro
  | InstSeq (Inst name typ) (Inst name typ)

deriving instance (Show (anno (Expr typ anno name)), Show (anno (Pattern anno name)), Show typ, Show name) => Show (Expr typ anno name)
deriving instance (Eq (anno (Expr typ anno name)), Eq (anno (Pattern anno name)), Eq typ, Eq name) => Eq (Expr typ anno name)

-- | pattern matching syntax
data Pattern anno name
  = PatWild         -- ^ match every thing and ignore it
  | PatUnit         -- ^ unit pattern
  | PatRef name     -- ^ bind content to a variable or match a constructor
  | PatSym name     -- ^ match polymorphic variant symbol
  | PatLit Literal  -- ^ match builtin literal value, numbers and strings and unit
  | PatTup [Pattern anno name]          -- ^ tuple pattern
  | PatRec [(name, Pattern anno name)]  -- ^ record pattern, open or closed
  | PatSum (Pattern anno name)          -- ^ variant pattern, both for **polymorphic variant** and **closed variant**
           [Pattern anno name]
  | PatView name (Pattern anno name)    -- ^ allow applying function to the argument and view results
  | PatBind name (Pattern anno name)    -- ^ assign a name to the whold pattern
  | PatAnno (anno (Pattern anno name))  -- ^ type annotation for pattern

deriving instance (Show (anno (Pattern anno name)), Show name) => Show (Pattern anno name)
deriving instance (Eq (anno (Pattern anno name)), Eq name) => Eq (Pattern anno name)

-- | pattern group with logic combination
data GPattern anno name
  = Pattern (Pattern anno name)   -- ^ pattern for single variable
  | PatSeq [GPattern anno name]   -- ^ **pat1 | pat2**, match with sequential patterns, one by one, capture only one of them
  | PatGrp [GPattern anno name]   -- ^ **pat1, pat2, pat3**, match with multiple patterns simutaneously, capture all of them at a time.
                                  --   some special rules will be applied to it. thus, called pattern group.

deriving instance (Show (anno (Pattern anno name)), Show name) => Show (GPattern anno name)
deriving instance (Eq (anno (Pattern anno name)), Eq name) => Eq (GPattern anno name)

-- | field for record
data Field label typ
  = Field label typ
  | RowOf typ
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Lambda computation block, support both light and heavy notation
data Lambda typ anno name
  = Lambda [Bound typ name]
    (GPattern anno name, Expr typ anno name)
    [(GPattern anno name, Expr typ anno name)]

deriving instance (Show (anno (Expr typ anno name)), Show (anno (Pattern anno name)), Show name, Show typ) => Show (Lambda typ anno name)
deriving instance (Eq (anno (Expr typ anno name)), Eq (anno (Pattern anno name)), Eq name, Eq typ) => Eq (Lambda typ anno name)

$(deriveBifunctor ''Field)

makeBaseFunctor [d| instance Traversable anno => Recursive (Expr typ anno name) |]
makeBaseFunctor [d| instance Traversable anno => Recursive (Pattern anno name) |]
makeBaseFunctor [d| instance Traversable anno => Recursive (GPattern anno name) |]
