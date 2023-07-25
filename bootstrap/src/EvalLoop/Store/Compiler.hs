module EvalLoop.Store.Compiler
  (

    ShellCompilerStore (..)
  , stageStore
  , operators
  , jitModules
  , funSymbols
  , module Compiler.Store
  )
where

import Language.Core (Name, OperatorStore)
import Compiler.Store

import LLVM.Module (Module)

import Control.Lens

data ShellCompilerStore
  = ShellCompilerStore
    { _stageStore :: StageStore
    , _operators :: OperatorStore
    , _jitModules :: (Name, Module)
    , _funSymbols :: [Name]
    }

makeLenses ''ShellCompilerStore
