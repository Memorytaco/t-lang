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

  -- * useful alias for surface language

  -- ** surface lang for type
    TypSurface

  , TypSurfaceExt
  , TypSurfaceBndExt

  -- ** surface lang for pattern
  , PatSurface
  , PatSurfaceLitExt

  , GPatSurface
  , GPatSurfaceLitExt

  -- ** surface lang for expr
  , ExprSurface
  , ExprSurfaceExt

  -- ** surface macro lang
  , MacroSurfaceAttr
  , MacroSurfaceAttrExt

  -- ** parsed declaration structure
  , DeclSurface
  , DeclStoreSurface
  , DeclSurfaceExt

  -- ** parsed module
  , ModuleSurface
  , ModuleSurfaceExt

  -- ** graphic type and type constraint

  , GraphicTypeSurface
  , GraphicNodesSurface
  , GraphicEdgesSurface

  , ConstraintSurface
  , ConstraintNodesSurface
  , ConstraintEdgesSurface

  -- * useful alias for internal language

  -- ** internal pattern
  , PatInternal
  , PatInternalLitExt

  -- ** internal expersion
  , ExprInternal
  , ExprInternalExt

  -- * useful alias for sugar definition
  --
  -- we place handful type alias here to help
  -- writing transformation on expression.

  , SugarLambdaType
  , SugarLambdaTerm

  , SugarLetRecur
  , SugarLetTerm

  -- ** re-export of structure
  , module Expr
  , module Module
  , module Type
  , module Operator
  , module Decl
  , module Pattern
  , module Name
  , module Macro
  , module Constraint
  , module Utility

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
import Language.Core.Macro as Macro
import Language.Core.Constraint as Constraint
import Language.Core.Utility as Utility

-- ** required imports
import Language.Core.Extension as Ext
import Language.Generic
import Language.DataLayout (Rep (..))
import Data.Text (Text)
import Graph.Extension.GraphicType
import Graph.Core (CoreG)

-----------------------------------------
-----------------------------------------
-- * Surface definition of core structure
-----------------------------------------
-----------------------------------------

------------------
-- ** Surface type
------------------

type TypSurface = Type TypSurfaceBndExt TypSurfaceExt Name Name
type TypSurfaceExt
  = Tuple :+: Record Label :+: Variant Label
  :+: Ext.Literal Integer :+: Ext.Literal Text :+: Rep
type TypSurfaceBndExt = Forall Prefix :++: Scope Prefix

---------------------
-- ** Surface Pattern
---------------------

type PatSurface typ = Pattern PatSurfaceLitExt ((:::) typ) Label Name
type PatSurfaceLitExt = LiteralText :+: LiteralInteger :+: LiteralNumber

type GPatSurface typ = Pattern GPatSurfaceLitExt ((:::) typ :+: PatGroup) Label Name
type GPatSurfaceLitExt = PatSurfaceLitExt

------------------
-- ** Surface Expr
------------------

-- | Surface Expr
type ExprSurface typ = Expr (ExprSurfaceExt typ) Name
type ExprSurfaceExt typ =
      LetGrp (PatSurface typ)                             -- plain pattern match via "let" binding
  :+: Let (Binder (Name ::: Maybe typ))                   -- desugared non-recursive "let" binding
  :+: Letrec (Binder (Name ::: Maybe typ))                -- desugared possible-recrusive "let" binding group
  :+: Equation (GPatSurface typ) (Prefixes Name typ)      -- heavy lambda
  :+: Equation (Grp (PatSurface typ)) (Prefixes Name typ) -- light lambda
  :+: Apply     -- application, term beta-reduction or type application
  :+: Value typ -- unlifted type value
  :+: (:::) typ -- type annotation
  :+: LiteralText :+: LiteralInteger :+: LiteralNumber
  :+: Tuple :+: Record Label :+: Selector Label :+: Constructor Label


-------------------
-- ** Surface Attribute Macro
-------------------
type MacroSurfaceAttr a = Macro MacroSurfaceAttrExt a
type MacroSurfaceAttrExt =
      LiteralText       -- allow string value in macro
  :+: LiteralInteger    -- allow integer value in macro
  :+: List              -- allow list value in macro
  :+: Constructor Name  -- allow application in macro
  :+: Record Name       -- allow key value pair in macro

----------------------------------
-- ** Surface toplevel declaration
----------------------------------

-- | this is what structure we used to do name occurrence checking at stage1
--
-- Every structure is parameterised by `Name` for querying
type DeclSurface = Decl (DeclSurfaceExt TypSurface (ExprSurface TypSurface)) Name
type DeclStoreSurface = DeclStore (DeclSurfaceExt TypSurface (ExprSurface TypSurface)) Name
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


----------------------------------------------------------------------
  --  ** Surface Graphic Type and Constraint definition
----------------------------------------------------------------------

-- | Graph representation of type
type GraphicTypeSurface = CoreG GraphicNodesSurface GraphicEdgesSurface Int
type GraphicNodesSurface =
      NodePht :+: T NodeBot :+: T (NodeLit Integer) :+: T (NodeLit Text)
  :+: T NodeTup :+: T NodeSum :+: T NodeRec :+: T (NodeRef Name)
  :+: T NodeApp :+: T (NodeHas Label) :+: T NodeArr
type GraphicEdgesSurface = T Sub :+: T (Binding Name)

-- | Graph representation of constraint
type ConstraintSurface = CoreG ConstraintNodesSurface ConstraintEdgesSurface Int
type ConstraintNodesSurface =
  GraphicNodesSurface
  :+: NDOrder :+: R [] :+: G
type ConstraintEdgesSurface =
  GraphicEdgesSurface
  :+: Pht O :+: Pht Sub :+: Pht NDOrderLink
  :+: T Unify :+: T Instance

------------------------------------------
------------------------------------------
-- * Internal definition of core structure
------------------------------------------
------------------------------------------


----------------------------------
-- ** Internal expression
----------------------------------

type PatInternal typ = Pattern PatInternalLitExt (Cast typ :+: PatGroup) Label Name
type PatInternalLitExt = LiteralText :+: LiteralInteger :+: LiteralNumber

type ExprInternal typ = Expr (ExprInternalExt typ)
type ExprInternalExt typ
  =   Let (Binder (Name ::: typ))        -- non-recursive local binding
  :+: Letrec (Binder (Name ::: typ))     -- recursive local binding group
  :+: Lambda (Binder (Prefix Name typ)) -- type lambda
  :+: Lambda (Binder (Name ::: typ))     -- term lambda
  :+: Apply                             -- term application
  :+: Match (PatInternal typ)           -- pattern match
  :+: Value (Coerce Name typ)           -- evidence of type inference
  :+: Cast typ                          -- type coercion
  :+: LiteralText :+: LiteralInteger :+: LiteralNumber
  :+: Tuple :+: Record Label :+: Selector Label :+: Constructor Label


------------------------------------------
------------------------------------------
-- * Sugar definition of core structure
------------------------------------------
------------------------------------------

-- ** Sugar /\ lambda extension
type SugarLambdaType typ = Lambda (Binder (Prefix Name typ))

-- ** Sugar term lambda, which may or may not include type annotation
type SugarLambdaTerm typ = Lambda (Binder (Name ::: Maybe typ))

-- ** Sugar recursive "Let" binding group, all allow optional type annotation
type SugarLetRecur typ = Letrec (Binder (Name ::: Maybe typ))

-- ** Sugar normal "Let" binding group
type SugarLetTerm typ = Let (Binder (Name ::: Maybe typ))
