module Tlang.AST.Expression
  ( Expr (..)
  , ExprF (..)
  , LitValue (..)
  , LambdaBlock (..)

  , getExprName
  )
where

import Data.Functor.Foldable.TH

import Tlang.Parser.Pratt
import Tlang.AST.Type (TypAnno)

-- Expression, parametric with binary operator and also one name tag.
-- TODO: extend or refactor the expression structure to hold lambda;
data Expr op name = ExLit name LitValue -- literal value
                  | ExRef name          -- variable or term name reference
                  | ExCall name (Expr op name) (Expr op name)     -- application
                  | ExBind name (Maybe TypAnno) (Expr op name)    -- variable binding
                  | ExOp op name (Expr op name) (Expr op name)    -- binary operator
                  | ExOpUni op name (Expr op name)                -- uni operator
                  deriving (Show, Eq)

data LitValue = LitInt Integer | LitNumber Double | LitString String deriving (Show, Eq)

$(makeBaseFunctor ''Expr)

getExprName :: Expr op name -> name
getExprName (ExLit n _) = n
getExprName (ExRef n) = n
getExprName (ExCall n _ _) = n
getExprName (ExBind n _ _) = n
getExprName (ExOp _ n _ _) = n
getExprName (ExOpUni _ n _) = n

-- function block will be represented by a lambda, a function definition is no more than assign a name along with type
-- annotation to a lambda expression.
-- TODO: allow user to use lambda in expression.
data LambdaBlock name = LambdaBlock
    { lambdaVars :: [(name, Maybe TypAnno)]
    , lambdaExprs :: [Expr (Operator String) name]
    } deriving (Show, Eq)

