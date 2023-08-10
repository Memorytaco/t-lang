module Compiler.Store
  (

  -- ** Stage Store
    StageStore (..)
  , stageSourceParsing
  , stageNameChecking

  -- ** init value for Stage Store
  , initStageStore

  -- ** Parser stage for compiler
  , StageSourceParsing (..)
  , spSources
  , spFiles
  , spOutdates

  , emptyStageSourceParsing

  , StageNameChecking (..)
  , ncModules
  , ncOutdates

  , emptyStageNameChecking

  -- ** methods
  , UseCompilerStore
  , AccessCompilerStore
  , HasCompilerStore
  )
where

import Language.Core ( Name, ModuleSurface )

import Control.Lens ( makeLenses )
import Capability.Reader (HasReader)
import Capability.State (HasState)

import Data.Map (Map, empty)
import qualified Data.Set as Set
import Data.Text (Text)

-- | a global store for holding data in different stages of compiler
data StageStore
  = StageStore
    { _stageSourceParsing :: StageSourceParsing
    , _stageNameChecking :: StageNameChecking
    }

initStageStore :: StageStore
initStageStore = StageStore emptyStageSourceParsing emptyStageNameChecking

------------------------------------
-- | SourceParsing stage for compiler
------------------------------------

data StageSourceParsing
  = StageSourceParsing
    { -- | modules which are parsed with no problem, with operator resolved and file path tracked
      _spSources :: [(FilePath, ModuleSurface)]
      -- | the source file loaded with mangled module name as key
    , _spFiles  :: Map Name Text
      -- | track outdated module content
    , _spOutdates :: Map Name FilePath
    }

emptyStageSourceParsing :: StageSourceParsing
emptyStageSourceParsing = StageSourceParsing [] empty empty

------------------------------------
-- | NameChecking Stage for compiler
--
-- its fields are prefixed with "nc".
------------------------------------

data StageNameChecking
  = StageNameChecking
    { -- | modules which are checked and have names mangled.
      _ncModules :: Map Name ModuleSurface
      -- | modules which need rechecking
    , _ncOutdates :: Set.Set Name
    }

emptyStageNameChecking :: StageNameChecking
emptyStageNameChecking = StageNameChecking empty Set.empty

makeLenses ''StageNameChecking
makeLenses ''StageSourceParsing
makeLenses ''StageStore

-- | marker which indicates there is a compiler store somewhere.
--
-- Implementation of CompilerStore may get changed
data UseCompilerStore
-- | we have an environment contained the store.
type AccessCompilerStore m = (HasReader UseCompilerStore StageStore m)
-- | we have an environment contained the store and are allowed to modify it.
--
-- We can have the invariant store as a reference.
type HasCompilerStore m = (HasState UseCompilerStore StageStore m, AccessCompilerStore m)