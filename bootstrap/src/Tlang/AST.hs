module Tlang.AST
  (

    -- ** useful data
    Label (..)

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
  , module Name

  )
where

import Tlang.AST.Expr as Expr
import Tlang.AST.Module as Module
import Tlang.AST.Type as Type
import Tlang.AST.Operator as Operator
import Tlang.AST.Decl as Decl
import Tlang.AST.Pattern as Pattern
import Tlang.AST.Name as Name

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

-- | label for variant and record
newtype Label = Label Text deriving (Eq, Ord) deriving IsString via Text
instance Show Label where
  show (Label text) = unpack text

