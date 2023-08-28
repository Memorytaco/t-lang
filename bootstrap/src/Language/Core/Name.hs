module Language.Core.Name
  (

    Name (..)
  , _Name

  , NameSpace (..)
  , _NameSpace

  , QName (..)
  , _QName, _QNameSpace

  , Alias (..)
  , getAliasName
  , aliasName
  , isSameAlias

  , Label (..)
  , _Label
  )
where

import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text, unpack)
import Prettyprinter (Pretty)
import Control.Lens ( makeLenses )

-- | a wrapper for name reference
newtype Name = Name { __Name :: Text }
  deriving (Eq, Ord)
  deriving (IsString, Semigroup, Monoid, Pretty) via Text
instance Show Name where
  show (Name text) = unpack text
makeLenses ''Name

newtype NameSpace
  = NameSpace { __NameSpace :: Text }
  deriving (Eq, Ord)
  deriving (IsString, Semigroup, Monoid, Pretty) via Text
instance Show NameSpace where
  show (NameSpace text) = unpack text
makeLenses ''NameSpace

-- | qualified name
data QName name
  = QName { __QNameSpace :: NameSpace
          , __QName :: name
          } deriving (Eq, Ord, Functor)
instance Show name => Show (QName name) where
  show (QName q n) = show q <> "/" <> show n
makeLenses ''QName

-- | name alias, a wraper for name
data Alias name
  = Alias name (Maybe name)
  deriving (Eq, Ord, Functor)

-- | get name from aliased name
getAliasName :: Alias name -> name
getAliasName (Alias name alias'maybe) = fromMaybe name alias'maybe

-- | give alias "b" to name "a".
--
-- Now we can use "b" to represent "a" if we invoke `aliasName a b`
aliasName :: name -> name -> Alias name
aliasName origin alias = Alias origin (Just alias)

-- | test if two alias are all aliased to same name.
isSameAlias :: Eq name => Alias name -> Alias name -> Bool
isSameAlias (Alias a _) (Alias b _) = a == b

instance Show name => Show (Alias name) where
  show (Alias name (Just alias)) = show name <> " as " <> show alias
  show (Alias name Nothing) = show name

-- | label which is used to distinguish between `Name`
newtype Label = Label { __Label :: Text }
  deriving (Eq, Ord)
  deriving (IsString, Pretty) via Text
instance Show Label where
  show (Label text) = unpack text
makeLenses ''Label

