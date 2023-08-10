module EvalLoop.Store
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
import GHC.Generics (Generic)

-- | information related to runtime compilation and code evaluation
data EvalState
  = EvalState
    { _evalStore :: EvalCompilerStore
    , _sharedLibs :: [FilePath]
    , _objectFiles :: [FilePath]
    , _linesNumber :: Integer
    } deriving Generic

makeLenses ''EvalState

-- | create new state for evaluation
newEvalState :: MonadIO m => m EvalState
newEvalState = do
  compilerStore <- newEvalCompilerStore (Name "repl:0") "repl:0"
  return $ EvalState compilerStore [] [] 0

