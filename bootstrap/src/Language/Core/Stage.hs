module Language.Core.Stage
  ( CoreStage (..)
  )
where

data CoreStage
  = SParsing  -- ^ parsing stage, building syntax trees
  | SBytecode -- ^ compiled bytecode, a middle state
  deriving (Show, Eq, Ord)
