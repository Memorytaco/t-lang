module Tlang.AST.Name
  ( UnTypedName (..)
  , TypedName (..)

  , getTyped
  )
where

import Tlang.AST.Type (TypAnno (..))

-- | a name could be provided with a type annotation or none but the representation
-- is consistent.
data UnTypedName op = UnTypedName (TypAnno op) String
                    | UnTyped deriving (Show, Eq)

-- | Every expression or term is marked with a type.
data TypedName t = TypedName t String Integer
                 | Typed t
                 deriving (Eq, Functor)

instance Show a => Show (TypedName a) where
  show (TypedName a n _) = n <> ": " <> show a
  show (Typed a) = show a

getTyped :: TypedName a -> a
getTyped (TypedName a _ _) = a
getTyped (Typed a) = a

