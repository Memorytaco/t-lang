module EvalLoop.Store.Compiler
  (

    EvalCompilerStore (..)
  , stageStore
  , operators
  , compiledModules
  , evalJITSession

  , jitContext
  , jitClass
  , jitSessions
  , module Compiler.Store
  )
where

import Language.Core (Name, OperatorStore)
import Compiler.Store

import JIT.LLVM as JIT

import LLVM.Module (Module)

import Control.Lens

data EvalCompilerStore
  = EvalCompilerStore
    { _stageStore :: StageStore
    , _operators  :: OperatorStore
    , _compiledModules :: [(Name, Module)]
    , _evalJITSession :: EvalJITSession
    }

data EvalJITSession
  = EvalJITSession
    { _jitContext :: JIT.LLVMJITContext -- ^ main context, to which ShellJITSession's lifetime is bound
    , _jitClass   :: JIT.JITClass -- ^ current class, in which symbols are linked and executed
    , _jitSessions :: [String]  -- ^ available JITClasses, with a unique name for each
    }

makeLenses ''EvalJITSession
makeLenses ''EvalCompilerStore
