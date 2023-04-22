module Tlang.AST
  ( module Expr
  , module Module
  , module Type
  , module Operator

  , None (..)
  , Symbol (..)
  , Label (..)

  , StandardType
  , StandardRepType
  , StandardMixType

  , TypeAST
  , TypLitExt
  , TypBindExt
  )
where

import Tlang.AST.Expr as Expr
import Tlang.AST.Module as Module
import Tlang.AST.Type as Type
import Tlang.AST.Operator as Operator
import Data.GraphViz
import Tlang.Extension.Type as Ext
import Tlang.Generic
import Tlang.Rep (DataRep, SeqT)
import Data.Functor.Const (Const)

type TypLitExt label = Tuple :+: Record label :+: Variant label :+: Const Ext.Literal
type TypBindExt bound = Forall bound :+: Scope bound

-- | original type definition
type StandardType name label b inj = Type name (TypLitExt label) (TypBindExt b) inj
-- | type representation for standard type definition
type StandardRepType name label b inj = DataRep SeqT (StandardType name label b inj)
type StandardMixType name label b inj = StandardType name label b inj (StandardRepType name label b inj)

-- | representation used for Type in AST
type TypeAST inj = StandardType Symbol Label (Bound Symbol) inj (StandardRepType Symbol Label (Bound Symbol) inj)

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

-- | label for variant and record
newtype Label = Label String deriving (Eq, Ord)
instance Show Label where
  show (Label s) = s

