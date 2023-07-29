module Compiler.Store
  (

    StageStore (..)
  , stageStore1
  , stageStore2

  , initStageStore

  , StageStore1 (..)
  , searchPath
  , focusModule1
  , focusMutualModules1
  , stage1Module

  , Stage1Module (..)
  , version1
  , loadedMList
  , loadedMFilePath

  , initStage1Module
  , initStageStore1

  , StageStore2 (..)
  , initStageStore2

  )
where

import Language.Core

import Control.Lens

import Data.Map (Map, empty)
import Data.Text (Text)

data StageStore
  = StageStore
    { _stageStore1 :: StageStore1
    , _stageStore2 :: StageStore2
    }

initStageStore :: StageStore
initStageStore = StageStore initStageStore1 initStageStore2

-------------------------------
-- Store for Compiler Stage 1
-------------------------------

data StageStore1
  = StageStore1
    { -- | module search path
      _searchPath :: [Text]
      -- | j
    , _focusModule1 :: Maybe (Name, ModuleSurface)
    , _focusMutualModules1 :: [(Name, ModuleSurface)]
    , _stage1Module :: Stage1Module
    }

data Stage1Module
  = Stage1Module
    { 
      -- | for compatible reason, defaults to (0,0,1)
      _version1 :: (Integer, Integer, Integer)
    , _loadedMList :: Map Name ModuleSurface
    , _loadedMFilePath :: Map Name Text
    }

initStage1Module :: Stage1Module
initStage1Module = Stage1Module (0, 0, 1) empty empty
initStageStore1 :: StageStore1
initStageStore1 = StageStore1 [] Nothing [] initStage1Module

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
makeLenses ''Stage1Module
makeLenses ''StageStore1
makeLenses ''StageStore
