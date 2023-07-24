module Language.Core.Attribute
  (
    Attr (..)
  )
where

import Data.Text (Text)

-- | a small expression for defining attributes
data Attr
  = AttrI Integer         -- ^ Integer value
  | AttrT Text            -- ^ literal value
  | AttrS [Attr]          -- ^ sequence items, take a list form
  | AttrC Text [Attr]     -- ^ constructor value, with optional arguments
  | AttrP [(Text, Attr)]  -- ^ associated value, take a record form
  deriving (Show, Eq, Ord)
