module Interface.Config
  ( ShellConfig (..)
  , SearchEnv (..)

  , ShellState (..)
  )
where

import Tlang.AST (OperatorStore)
import Data.Text (Text)

data ShellConfig = ShellConfig
  { env :: SearchEnv
  } deriving (Show, Eq)

data SearchEnv
  = SearchEnv
    { libPath :: [Text]
    , srcPath :: [Text]
    } deriving (Show, Eq)

data ShellState = ShellState
  { lineCount :: Int
  , operators :: OperatorStore
  } deriving (Show, Eq)

