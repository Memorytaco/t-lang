module Compiler.Store
  (

  -- ** Stage Store
    StageStore (..)
  , stageSourceParsing
  , stageStore2

  -- ** init value for Stage Store
  , initStageStore

  -- ** Parser stage for compiler
  , StageSourceParsing (..)
  , parsedSource
  , parsedFiles

  , emptyStageSourceParsing

  , StageStore2 (..)
  , initStageStore2

  )
where

import Language.Core

import Control.Lens

import Data.Map (Map, empty)
import Data.Text (Text)

-- | a global store for holding data in different stages of compiler
data StageStore
  = StageStore
    { _stageSourceParsing :: StageSourceParsing
    , _stageStore2 :: StageStore2
    }

initStageStore :: StageStore
initStageStore = StageStore emptyStageSourceParsing initStageStore2

-----------------------------------
-- SourceParsing stage for compiler
-----------------------------------

data StageSourceParsing
  = StageSourceParsing
    {
      -- | modules which are parsed with no problem, with operator resolved and file path tracked
      _parsedSource :: [(FilePath, ModuleSurface)]
      -- | the source file loaded with mangled module name as key
    , _parsedFiles  :: Map Name Text
    }

emptyStageSourceParsing :: StageSourceParsing
emptyStageSourceParsing = StageSourceParsing [] empty

-------------------------------
-- Store for Compiler Stage 2
-------------------------------

data StageStore2
  = StageStore2
    {
    }

initStageStore2 :: StageStore2
initStageStore2 = StageStore2

makeLenses ''StageStore2
makeLenses ''StageSourceParsing
makeLenses ''StageStore
