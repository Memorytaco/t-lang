module Language.Core.Extension
  (
    -- ** sharing extension
    LiteralText (..)
  , LiteralNatural (..)
  , LiteralInteger (..)
  , LiteralNumber (..)

    -- ** reexport other extensions
  , module Type
  , module Expr
  , module Decl
  , module Common
  )
where

import Data.Text (Text)

import Language.Core.Extension.Type as Type
import Language.Core.Extension.Expr as Expr
import Language.Core.Extension.Decl as Decl
import Language.Core.Extension.Common as Common

-- ** a cluster of literals
newtype LiteralText a = LiteralText (Literal Text a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype LiteralNatural a = LiteralNatural (Literal Integer a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype LiteralInteger a = LiteralInteger (Literal Integer a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
newtype LiteralNumber a = LiteralNumber (Literal Double a) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

