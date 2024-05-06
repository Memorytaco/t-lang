module Language.Core.Macro
  ( Attr (..)
  , Macro (..)
  , MacroF (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Language.TH (fixQ)

import Data.Text (Text)

-- | Structure is defined by macro functionality `f` and
-- result is defined by parameter `inst`, meaning
-- instruction. `inst` can also be used for other
-- temporary purpose for convenience.
--
-- Macro may or may not be inline, block or
-- special one. It may or may not involve
-- customized user syntax or reading environment
-- information.
data Macro f inst
  = Inf inst
  | Macro (f (Macro f inst))
  deriving (Functor, Foldable)

instance Functor f => Applicative (Macro f) where
  pure = Inf
  (Inf f) <*> (Inf a) = Inf (f a)
  f <*> Macro fv = Macro ((f <*>) <$> fv)
  (Macro fv) <*> a = Macro ((<*> a) <$> fv)

instance Functor f => Monad (Macro f) where
  Inf a >>= f = f a
  Macro fma >>= f = Macro ((>>= f) <$> fma)


-- | a small expression for defining attributes
data Attr
  = AttrI Integer         -- ^ Integer value
  | AttrT Text            -- ^ literal value
  | AttrS [Attr]          -- ^ sequence items, take a list form
  | AttrC Text [Attr]     -- ^ constructor value, with optional arguments
  | AttrP [(Text, Attr)]  -- ^ associated value, take a record form
  deriving (Show, Eq, Ord)

makeBaseFunctor $ fixQ [d|
  instance (Functor f) => Recursive (Macro f a)
  |]
