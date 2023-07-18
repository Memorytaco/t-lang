module Tlang.AST.Name
  ( Name (..)
  , NameSpace (..)
  , QName (..)
  , Alias (..)
  )
where

import Data.String (IsString)
import Data.Text (Text, unpack)

-- | a wrapper for name reference
newtype Name = Name Text
  deriving (Eq, Ord)
  deriving (IsString, Semigroup, Monoid) via Text
instance Show Name where
  show (Name text) = unpack text

newtype NameSpace
  = NameSpace Text
  deriving (Eq, Ord)
  deriving (IsString, Semigroup, Monoid) via Text
instance Show NameSpace where
  show (NameSpace text) = unpack text

-- | qualified name
data QName name = QName NameSpace name deriving (Eq, Ord, Functor)
instance Show name => Show (QName name) where
  show (QName q n) = show q <> "/" <> show n

-- | name alias, a wraper for name
data Alias name
  = Alias name (Maybe name)
  deriving (Eq, Ord, Functor)

instance Show name => Show (Alias name) where
  show (Alias name (Just alias)) = show name <> " as " <> show alias
  show (Alias name Nothing) = show name
