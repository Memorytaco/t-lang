module EvalLoop.Config
  ( ShellConfig (..)
  , SearchEnv (..)

  , ShellState (..)
  , EvalState (..)
  , compilerState
  , sharedLibs
  , objectFiles
  , linesNumber
  )
where

import Language.Core (OperatorStore, Module, Name)
import Data.Text (Text)
import EvalLoop.Store.Compiler
import Control.Lens

data ShellConfig = ShellConfig
  { env :: SearchEnv
  } deriving (Show, Eq)

data SearchEnv
  = SearchEnv
    { libPath :: [Text]
    , srcPath :: [Text]
    } deriving (Show, Eq)

data EvalState
  = EvalState
    { _compilerState :: EvalCompilerStore
    , _sharedLibs :: [FilePath]
    , _objectFiles :: [FilePath]
    , _linesNumber :: Integer
    }

makeLenses ''EvalState

data ShellState decls = ShellState
  { lineCount :: Int
  , operators :: OperatorStore
  , modules :: [Module decls Name]
  }

