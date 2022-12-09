module Tlang.AST.Expr
  ( Expr (..)
  , ExprF (..)
  , LitValue (..)
  , Pattern (..)
  , PatternF (..)
  , Lambda (..)
  , LambdaBranch (..)

  , getExprInfo
  )
where

import Data.Functor.Foldable.TH

-- | Expression, parametric with binary operator and also one info tag.
data Expr op info = ExLit info LitValue -- ^ literal value and its type
                  | ExRef info          -- ^ variable or term info reference
                  | ExBeta info (Expr op info) (Expr op info) -- ^ Lambda beta reduction, aka. function application
                  | ExBind info (info, (Expr op info)) (Expr op info) -- ^ local variable binding
                  | ExPatBind info ((Pattern info), Expr op info) (Expr op info) -- ^ local pattern binding
                  | ExOp op info (Expr op info) (Maybe (Expr op info)) -- ^ term level operator
                  | ExLambda info (Lambda op info) -- ^ We allow full power of lambda in expression
                  deriving (Show, Eq)

-- | simple literal value
data LitValue
  = LitInt Integer
  | LitNumber Double
  | LitString String
  | LitUnit -- ^ unit value, which is **()**
  -- | LitRecord TODO: not adding literal record until F :< system
  deriving (Show, Eq)

-- | simple pattern match
data Pattern info
  = PatWild -- ^ match every thing and ignore it
  | PatLit info LitValue -- ^ match literal value
  | PatVar info     -- ^ match variable info
  | PatRec info [(info, Pattern info)]
  | PatNam info [Pattern info] -- ^ match named type constructor
  deriving (Show, Eq)

-- | Lambda computation block
newtype Lambda op info
  = Lambda [LambdaBranch op info]
  deriving (Show, Eq)

-- | a pattern match branch
data LambdaBranch op info = LambdaBranch
  { lMatch :: [Pattern info]
  , lExpr  :: Expr op info
  } deriving (Show, Eq)

$(makeBaseFunctor ''Expr)
$(makeBaseFunctor ''Pattern)

getExprInfo :: Expr op info -> info 
getExprInfo (ExLit n _) = n
getExprInfo (ExRef n) = n
getExprInfo (ExBeta n _ _) = n
getExprInfo (ExBind n _ _) = n
getExprInfo (ExPatBind n _ _) = n
getExprInfo (ExOp _ n _ _) = n
getExprInfo (ExLambda n _) = n
