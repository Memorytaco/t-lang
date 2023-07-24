module EvalLoop.Config
  ( ShellConfig (..)
  , SearchEnv (..)

  , ShellState (..)
  )
where

import Language.Core (OperatorStore, Module, Name)
import Data.Text (Text)

data ShellConfig = ShellConfig
  { env :: SearchEnv
  } deriving (Show, Eq)

data SearchEnv
  = SearchEnv
    { libPath :: [Text]
    , srcPath :: [Text]
    } deriving (Show, Eq)

data ShellState decls = ShellState
  { lineCount :: Int
  , operators :: OperatorStore
  , modules :: [Module decls Name]
  }

deriving instance Show (decls Name) => Show (ShellState decls)
deriving instance Eq (decls Name) => Eq (ShellState decls)

