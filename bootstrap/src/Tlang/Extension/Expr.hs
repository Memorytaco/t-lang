module Tlang.Extension.Expr
  (
    -- ** expresion structure
    Let (..)
  , Lambda (..)
  , Apply (..)
  , Selector (..)
  , Constructor (..)

  , VisibleType (..)
  , TypeApply (..)

  , Inst (..)
  )
where


-- | local name binding
data Let binder expr
  = Let (binder expr) expr expr
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Lambda computation block, support both light and heavy notation
data Lambda pattern' prefix expr
  = Lambda prefix (pattern' expr, expr) [(pattern' expr, expr)]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | application
data Apply expr
  = Apply expr expr [expr]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | @.selector@, field projector
newtype Selector name e = Selector name deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
-- | @`Variant@ for polymorphic variant, variant constructor
data Constructor name e = Constructor name [e] deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | type instantiation
data Inst name typ
  = InstOne       -- identity
  | InstTyp typ   -- bottom
  | InstNew name  -- abstract
  | InstInside (Inst name typ)
  | InstUnder name (Inst name typ)
  | InstElim
  | InstIntro
  | InstSeq (Inst name typ) (Inst name typ)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data VisibleType typ e = VisibleType typ e deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | type application, this alters inner types of expression for codegen.
-- like reduce type variable of `Lambda`
data TypeApply op expr = TypeApply op expr deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
