module Compiler.Store
  (

  -- ** Stage Store
    StageStore (..)
  , parserStage
  , stageStore2

  -- ** init value for Stage Store
  , initStageStore

  -- ** Parser stage for compiler
  , ParserStage (..)
  , parsedSource
  , parsedFiles

  , emptyParserStage

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
    { _parserStage :: ParserStage
    , _stageStore2 :: StageStore2
    }

initStageStore :: StageStore
initStageStore = StageStore emptyParserStage initStageStore2

-------------------------------
-- Store for Praser in Compiler
-------------------------------

data ParserStage
  = ParserStage
    {
      -- | modules which are parsed with no problem, with operator resolved and file path tracked
      _parsedSource :: [(FilePath, ModuleSurface)]
      -- | the source file loaded with mangled module name as key
    , _parsedFiles  :: Map Name Text
    }

emptyParserStage :: ParserStage
emptyParserStage = ParserStage [] empty

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
makeLenses ''ParserStage
makeLenses ''StageStore
