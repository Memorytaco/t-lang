module Language.Core.Utility
  (

    type (+>) (..)
  , (:==) (..)

  )
where

import Data.Bifunctor.TH (deriveBifunctor)


infixr 3 +>

-- | utility for free and bound variables.
data bind +> free
  = Bind bind -- ^ introduce new bound variable
  | Free free -- ^ increase variable index, from (bind2 +> a) to (bind1 +> (bind2 +> a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
$(deriveBifunctor ''(+>))

instance Applicative ((+>) bind) where
  pure = Free
  Bind x <*> _ = Bind x
  Free f <*> a = f <$> a

instance Monad ((+>) bind) where
  Bind x >>= _ = Bind x
  Free a >>= f = f a

-- | definition
data name :== definition
  = name :== definition
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
$(deriveBifunctor ''(:==))