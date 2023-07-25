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

  -- * useful alias

  -- ** surface lang for type
    TypSurface

  , TypSurfaceExt
  , TypSurfaceBExt

  -- ** surface lang for pattern
  , PatSurface
  , PatSurfaceLitExt

  , GPatSurface
  , GPatSurfaceLitExt

  -- ** surface lang for expr
  , ExprSurface
  , ExprSurfaceExt

  -- ** parsed declaration structure
  , DeclSurface
  , DeclsSurface
  , DeclSurfaceExt

  -- ** parsed module
  , ModuleSurface
  , ModuleSurfaceExt

  -- ** re-export of structure
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

-- ** re-export
import Language.Core.Expr as Expr
import Language.Core.Module as Module
import Language.Core.Type as Type
import Language.Core.Operator as Operator
import Language.Core.Decl as Decl
import Language.Core.Pattern as Pattern
import Language.Core.Name as Name
import Language.Core.Attribute as Attribute
import Language.Core.Class as Class

-- ** required imports
import Language.Core.Extension as Ext
import Tlang.Constraint
import Tlang.Generic
import Tlang.Rep (Rep (..))
import Data.Text (Text)

-----------------------------------------
-----------------------------------------
-- * Surface definition of core structure
-----------------------------------------
-----------------------------------------

------------------
-- ** Surface type
------------------

type TypSurface = Type TypSurfaceBExt TypSurfaceExt Name Name
type TypSurfaceExt = Tuple :+: Record Label :+: Variant Label :+: Ext.LiteralNatural :+: Ext.LiteralText :+: Rep
type TypSurfaceBExt = Forall (Prefix Name) :+: Scope (Prefix Name)

---------------------
-- ** Surface Pattern
---------------------

type PatSurface typ = Pattern PatSurfaceLitExt ((@:) typ) Label Name
type PatSurfaceLitExt = LiteralText :+: LiteralInteger :+: LiteralNumber

type GPatSurface typ = Pattern GPatSurfaceLitExt ((@:) typ :+: PatGroup) Label Name
type GPatSurfaceLitExt = PatSurfaceLitExt

------------------
-- ** Surface Expr
------------------

-- | Surface Expr
type ExprSurface typ = Expr (ExprSurfaceExt typ) Name
type ExprSurfaceExt typ =
  (   Let (PatSurface typ)
  :+: Equation (GPatSurface typ) (Prefixes Name typ)
  :+: Equation (Grp (PatSurface typ)) (Prefixes Name typ)
  :+: Apply :+: Tuple :+: Record Label
  :+: LiteralText :+: LiteralInteger :+: LiteralNumber
  :+: Value typ :+: Selector Label :+: Constructor Label
  :+: (@:) typ
  )

----------------------------------
-- ** Surface toplevel declaration
----------------------------------

-- | this is what structure we used to do name occurrence checking at stage1
--
-- Every structure is parameterised by `Name` for querying
type DeclSurface = Decl (DeclSurfaceExt TypSurface (ExprSurface TypSurface)) Name
type DeclsSurface = Decls (DeclSurfaceExt TypSurface (ExprSurface TypSurface)) Name
type DeclSurfaceExt typ expr =
      Item (UserOperator Text)
  :+: Item (AliasType DataPrefix typ Name)
  :+: Item (FFI typ Name)
  :+: UserValue expr (Maybe typ)
  :+: Item (DataType DataPrefix (DataBody (DataNone :+: Identity :+: DataEnum Label :+: DataStruct Label)) typ Name)

----------------------------------
-- ** Parsed module definition
----------------------------------

-- | this is what structure we used to store raw ingrediants.
type ModuleSurface = Module (ModuleSurfaceExt TypSurface (ExprSurface TypSurface)) Name
type ModuleSurfaceExt typ expr = DeclSurfaceExt typ expr

