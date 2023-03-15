module Tlang.AST
  ( module Expr
  , module Module
  , module Type
  , module Operator
  , None (..)
  , Symbol (..)
  )
where

import Tlang.AST.Expr as Expr
import Tlang.AST.Module as Module
import Tlang.AST.Type as Type
import Tlang.AST.Operator as Operator
import Data.GraphViz

-- | a place holder for every parametric data type
data None a = None deriving (Show, Eq, Functor, Foldable, Traversable)

-- | used to represent type name reference, both for operator and normal name
data Symbol = Symbol String | Op String deriving (Eq)
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

