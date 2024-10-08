{- | * Common utilities and settings
-- 
-- this module enforces some rules on the project
-- and most of them are monad settings.
-}
module Language.Setting
  (

    HasGraphShow

  -- ** Allow creating nodes
  , HasNodeCreator
  , NodeCreator
  , newNodeCounter
  , node

  -- ** Allow read and modify graph
  -- *** Grpah reader
  , HasGraphReader
  , GraphReader
  , askGraph
  , asksGraph
  , localGraph

  -- *** Graph state
  , HasGraph
  , GraphState
  , modifyGraph
  , getGraph
  , putGraph
  , getsGraph

  -- ** Allow generating true random name
  , HasRandomName
  , randomName
  , randomNameHint
  )
where

import Graph.Core (CoreG, Hole, Link, hole)
import Language.Generic ((:<:))

import Capability.State (HasState, modify, get, put, gets)
import Capability.Reader (HasReader, ask, asks, local)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor
import Data.Char (isAlphaNum)
import Data.String (IsString (..))
import System.Random.Stateful

type HasGraphShow nodes edges info = (Show (Link edges), Show (Hole nodes info), Show info)

-- | common environment for creating node
data NodeCreator
type HasNodeCreator m = (HasState NodeCreator Int m)

newNodeCounter :: HasNodeCreator m => m Int
newNodeCounter = modify @NodeCreator (+1) >> get @NodeCreator

-- | just return a new node with valid tag.
node :: (node :<: nodes, HasNodeCreator m) => node (Hole nodes Int) -> m (Hole nodes Int)
node val = newNodeCounter <&> hole val

-- | graph monad reader
data GraphReader
type HasGraphReader nodes edges info m = (HasReader GraphReader (CoreG nodes edges info) m)

-- | graph monad state modifier
data GraphState
type HasGraph nodes edges info m = (HasState GraphState (CoreG nodes edges info) m)

-- | read graph
askGraph :: HasGraphReader nodes edges info m => m (CoreG nodes edges info)
askGraph = ask @GraphReader

-- | read sub graph
asksGraph :: HasGraphReader nodes edges info m => (CoreG nodes edges info -> a) -> m a
asksGraph = asks @GraphReader

localGraph
  :: HasGraphReader nodes edges info m
  => (CoreG nodes edges info -> CoreG nodes edges info) -> m a -> m a
localGraph = local @GraphReader

modifyGraph :: HasGraph nodes edges info m => (CoreG nodes edges info -> CoreG nodes edges info) -> m ()
modifyGraph = modify @GraphState

putGraph :: HasGraph nodes edges info m => CoreG nodes edges info -> m ()
putGraph = put @GraphState

getGraph :: HasGraph nodes edges info m => m (CoreG nodes edges info)
getGraph = get @GraphState

getsGraph :: HasGraph nodes edges info m => (CoreG nodes edges info -> a) -> m a
getsGraph = gets @GraphState

type HasRandomName m = (StatefulGen (AtomicGenM StdGen) m, MonadIO m)

-- | generate a random name for anything with a `IsString` instance
randomName :: (IsString name, HasRandomName m)
           => Int    -- ^ name length
           -> m name -- ^ a random name
randomName n = fromString <$> replicateM n seed
  where seed = uniformRM ('0', 'Z') globalStdGen
           >>= \c -> if isAlphaNum c then return c else seed

-- | use a name as prefix to generate new random name
randomNameHint :: (Semigroup name, IsString name, HasRandomName m) => Int -> name -> m name
randomNameHint n name = (name <>) <$> randomName n
