{- | Common settings for projects
-}
module Language.Setting
  (

  -- ** Allow creating nodes
    HasNodeCreator
  , NodeCreator
  , newNodeCounter
  , node
  
  -- ** Allow read and modify graph
  , HasGraphReader
  , GraphReader
  , askGraph
  , asksGraph
  , localGraph

  , HasGraph
  , GraphState
  , modifyGraph
  , getGraph
  , getsGraph
  )
where
import Capability.State (HasState, modify, get, gets)
import Graph.Core (CoreG, Hole, hole)
import Capability.Reader (HasReader, ask, asks, local)
import Language.Generic ((:<:))
import Data.Functor

data NodeCreator
type HasNodeCreator m = (HasState NodeCreator Int m)

newNodeCounter :: HasNodeCreator m => m Int
newNodeCounter = modify @NodeCreator (+1) >> get @NodeCreator

node :: (node :<: nodes, HasNodeCreator m) => node (Hole nodes Int) -> m (Hole nodes Int)
node val = newNodeCounter <&> hole val

data GraphReader
type HasGraphReader nodes edges info m = (HasReader GraphReader (CoreG nodes edges info) m)

data GraphState
type HasGraph nodes edges info m = (HasState GraphState (CoreG nodes edges info) m)

askGraph :: HasGraphReader nodes edges info m => m (CoreG nodes edges info)
askGraph = ask @GraphReader

asksGraph :: HasGraphReader nodes edges info m => (CoreG nodes edges info -> a) -> m a
asksGraph = asks @GraphReader

localGraph
  :: HasGraphReader nodes edges info m
  => (CoreG nodes edges info -> CoreG nodes edges info) -> m a -> m a
localGraph = local @GraphReader

modifyGraph :: HasGraph nodes edges info m => (CoreG nodes edges info -> CoreG nodes edges info) -> m ()
modifyGraph = modify @GraphState

getGraph :: HasGraph nodes edges info m => m (CoreG nodes edges info)
getGraph = get @GraphState

getsGraph :: HasGraph nodes edges info m => (CoreG nodes edges info -> a) -> m a
getsGraph = gets @GraphState