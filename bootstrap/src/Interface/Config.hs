module Interface.Config
  ( ShellConfig (..)
  , SearchEnv (..)

  , ShellState (..)
  , addTermOperator
  , addTypeOperator
  )
where

import Tlang.AST (Operator)
import Data.Bifunctor (first, second)
import Data.List (nub)

data ShellConfig = ShellConfig
  { env :: SearchEnv
  } deriving (Show, Eq)

data SearchEnv
  = SearchEnv
    { libPath :: [String]
    , srcPath :: [String]
    } deriving (Show, Eq)

data ShellState = ShellState
  { lineCount :: Int
  , operators :: ([Operator String], [Operator String])
  } deriving (Show, Eq)

addTermOperator :: Operator String -> ShellState -> ShellState
addTermOperator op stat = stat { operators = second (nub . (op:)) $ operators stat }
addTypeOperator :: Operator String -> ShellState -> ShellState
addTypeOperator op stat = stat { operators = first (nub . (op:)) $ operators stat }
