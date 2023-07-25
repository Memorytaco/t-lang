module Compiler.Store
  (

    StageStore (..)
  , stageStore1
  , stageStore2

  , StageStore1 (..)
  , searchPath
  , focusOn
  , stage1Module

  , Stage1Module (..)
  , version1
  , loadedMList
  , loadedMFilePath

  , StageStore2 (..)

  )
where

import Language.Core

import Control.Lens

import Data.Map (Map)
import Data.Text (Text)

data StageStore
  = StageStore
    { _stageStore1 :: StageStore1
    , _stageStore2 :: StageStore2
    }

data StageStore1
  = StageStore1
    { _searchPath :: [Text] -- module search path
    , _focusOn :: Maybe (Name, ModuleSurface)
    , _stage1Module :: Stage1Module
    }

data Stage1Module
  = Stage1Module
    { _version1 :: (Integer, Integer, Integer) -- for compatible reason, defaults to (0,0,1)
    , _loadedMList :: Map Name ModuleSurface
    , _loadedMFilePath :: Map Name Text
    }

data StageStore2
  = StageStore2
    {
    }

makeLenses ''StageStore2
makeLenses ''Stage1Module
makeLenses ''StageStore1
makeLenses ''StageStore
