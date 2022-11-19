module Tlang.AST.Module
  ( Module (..)
  , ModuleNameSpace (..)
  , ModuleElement (..)

  , getLangModEleNamePair
  , getLangModEleBlock
  )
where

import Tlang.Parser.Pratt
import Tlang.AST.Expression

-- | language module definition
data Module deps defs = Module ModuleNameSpace deps defs deriving (Show, Eq)

newtype ModuleNameSpace = ModuleNameSpace { getModuleNameSpace :: String } deriving (Eq, Ord)
instance Show ModuleNameSpace where
  show (ModuleNameSpace n) = n


-- toplevel definition in a module
data ModuleElement name anno
  = ModuleFunction name (Maybe anno) (Maybe (LambdaBlock name))
  | ModuleBinding  name (Maybe anno) (Expr (Operator String) name) -- name binding: for global constant or function alias
  | ModuleOperator (Operator String) -- for user defined operator
  | ModuleType     name anno  -- TODO, the definition here is not completed
  | ModuleUnsafe   name anno  -- TODO, the definition here is not completed
  deriving (Show, Eq)


getLangModEleNamePair :: ModuleElement name anno -> (name, Maybe anno)
getLangModEleNamePair (ModuleFunction name anno _) = (name, anno)
getLangModEleNamePair (ModuleBinding name anno _) = (name, anno)
getLangModEleNamePair (ModuleType name anno) = (name, Just anno)
getLangModEleNamePair (ModuleUnsafe name anno) = (name, Just anno)

getLangModEleBlock :: ModuleElement name anno -> Maybe (LambdaBlock name)
getLangModEleBlock (ModuleFunction _ _ block) = block
getLangModEleBlock _ = Nothing
getLangModEleExpr :: ModuleElement name anno -> Maybe (Expr (Operator String) name)
getLangModEleExpr (ModuleBinding _ _ v) = Just v
getLangModEleExpr _ = Nothing

