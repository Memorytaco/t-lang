{- | Module which holds structures used to represent syntax tree and
    annotation.

    This module exports all core definition of syntax structure but
    not extensions. For extensions, you need to import `Core.Extension`
    for all Extensions or import specific symbols in "Core/Extension"
    directories.

    It also defines structure for surface language and internal language.
-}
module Language.Core
  (

    -- ** useful data
    StandardType

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
  , module Attribute
  , module Class

  )
where

import Language.Core.Expr as Expr
import Language.Core.Module as Module
import Language.Core.Type as Type
import Language.Core.Operator as Operator
import Language.Core.Decl as Decl
import Language.Core.Pattern as Pattern
import Language.Core.Name as Name
import Language.Core.Attribute as Attribute
import Language.Core.Class as Class

import Language.Core.Extension as Ext
import Tlang.Generic
import Tlang.Rep (Rep (..))

type TypPlugin label = Tuple :+: Record label :+: Variant label :+: Ext.LiteralNatural :+: Ext.LiteralText :+: Rep
type TypBindPlugin bound = Forall bound :+: Scope bound

-- | original type definition
type StandardType label bound = Type (TypBindPlugin bound) (TypPlugin label)

