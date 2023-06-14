module Interface.Config
  ( ShellConfig (..)
  , SearchEnv (..)

  , ShellState (..)
  , addTermOperator
  , addTermOperators
  , addTypeOperator
  )
where

import Tlang.AST (Operator)
import Data.Bifunctor (first, second)
import Data.List (nub)
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
  , operators :: ([Operator Text], [Operator Text])
  } deriving (Show, Eq)

addTermOperator :: Operator Text -> ShellState -> ShellState
addTermOperator op stat = stat { operators = first (nub . (op:)) $ operators stat }
addTermOperators :: [Operator Text] -> ShellState -> ShellState
addTermOperators ops stat = stat { operators = first (nub . (ops <>)) $ operators stat }
addTypeOperator :: Operator Text -> ShellState -> ShellState
addTypeOperator op stat = stat { operators = second (nub . (op:)) $ operators stat }
