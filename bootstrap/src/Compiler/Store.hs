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

  , StageNameChecking (..)
  , checkedModule

  , emptyStageNameChecking

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
    , _stageStore2 :: StageNameChecking
    }

initStageStore :: StageStore
initStageStore = StageStore emptyStageSourceParsing emptyStageNameChecking

------------------------------------
-- | SourceParsing stage for compiler
------------------------------------

data StageSourceParsing
  = StageSourceParsing
    { -- | modules which are parsed with no problem, with operator resolved and file path tracked
      _parsedSource :: [(FilePath, ModuleSurface)]
      -- | the source file loaded with mangled module name as key
    , _parsedFiles  :: Map Name Text
    }

emptyStageSourceParsing :: StageSourceParsing
emptyStageSourceParsing = StageSourceParsing [] empty

------------------------------------
-- | NameChecking Stage for compiler
------------------------------------

data StageNameChecking
  = StageNameChecking
    { -- | modules
      _checkedModule :: [Name]
    }

emptyStageNameChecking :: StageNameChecking
emptyStageNameChecking = StageNameChecking []

makeLenses ''StageNameChecking
makeLenses ''StageSourceParsing
makeLenses ''StageStore
