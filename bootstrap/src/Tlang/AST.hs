module Tlang.AST
  (

    -- ** useful data
    None (..)
  , Symbol (..)
  , Label (..)
  , Name (..)

  , StandardType

  , TypPlugin
  , TypBindPlugin


    -- ** reexport of AST
  , module Expr
  , module Module
  , module Type
  , module Operator
  , module Decl
  , module Pattern

  )
where

import Tlang.AST.Expr as Expr
import Tlang.AST.Module as Module
import Tlang.AST.Type as Type
import Tlang.AST.Operator as Operator
import Tlang.AST.Decl as Decl
import Tlang.AST.Pattern as Pattern

import Data.GraphViz
import Tlang.Extension.Type as Ext
import Tlang.Extension as Ext
import Tlang.Generic
import Tlang.Rep (Rep (..))

import Data.Text (Text, unpack)
import Data.String (IsString (..))

type TypPlugin label = Tuple :+: Record label :+: Variant label :+: Ext.LiteralNatural :+: Ext.LiteralText :+: Rep
type TypBindPlugin bound = Forall bound :+: Scope bound

-- | original type definition
type StandardType label bound = Type (TypBindPlugin bound) (TypPlugin label)

-- | a place holder for every parametric data type
data None a = None deriving (Show, Eq, Functor, Foldable, Traversable)

-- | used to represent type name reference, both for operator and normal name
data Symbol = Symbol String | Op String deriving (Eq)

-- | a wrapper for name reference
newtype Name = Name Text deriving (Eq, Ord) deriving IsString via Text
instance Show Name where
  show (Name text) = unpack text

-- | label for variant and record
newtype Label = Label Text deriving (Eq, Ord) deriving IsString via Text
instance Show Label where
  show (Label text) = unpack text

instance Show Symbol where
  show (Symbol s) = s
  show (Op s) = s

instance Ord Symbol where
  compare (Symbol s1) (Symbol s2) = compare s1 s2
  compare (Op s1) (Symbol s2) = compare s1 s2
  compare (Symbol s1) (Op s2) = compare s1 s2
  compare (Op s1) (Op s2) = compare s1 s2

instance Labellable Symbol where
  toLabelValue (Symbol name) = toLabelValue name
  toLabelValue (Op name) = toLabelValue name

