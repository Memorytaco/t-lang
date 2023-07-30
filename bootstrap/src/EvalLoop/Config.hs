module EvalLoop.Config
  (
    EvalState (..)
  , evalStore
  , sharedLibs
  , objectFiles
  , linesNumber

  , newEvalState

  , module EvalLoop.Store.Compiler
  )
where

import EvalLoop.Store.Compiler
import Control.Lens
import Control.Monad.IO.Class (MonadIO)
import Language.Core (Name (..))

data EvalState
  = EvalState
    { _evalStore :: EvalCompilerStore
    , _sharedLibs :: [FilePath]
    , _objectFiles :: [FilePath]
    , _linesNumber :: Integer
    }

makeLenses ''EvalState

-- | create new state for evaluation
newEvalState :: MonadIO m => m EvalState
newEvalState = do
  compilerStore <- newEvalCompilerStore (Name "repl:0") "repl:0"
  return $ EvalState compilerStore [] [] 0

