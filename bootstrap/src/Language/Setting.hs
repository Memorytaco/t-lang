{- | Common settings for projects
-}
module Language.Setting
  (

  -- ** Allow creating nodes
    HasNodeCreator
  
  -- ** Allow read and modify graph
  , HasGraphReader
  , askGraph
  , asksGraph

  , HasGraph
  )
where
import Capability.State (HasState)
import Graph.Core (CoreG)
import Capability.Reader (HasReader, ask, asks)

type HasNodeCreator m = (HasState "NodeCreator" Int m)

type HasGraphReader nodes edges info m = (HasReader "GraphReader" (CoreG nodes edges info) m)
type HasGraph nodes edges info m = (HasState "Graph" (CoreG nodes edges info) m)

askGraph :: HasGraphReader nodes edges info m => m (CoreG nodes edges info)
askGraph = ask @"GraphReader"

asksGraph :: HasGraphReader nodes edges info m => (CoreG nodes edges info -> a) -> m a
asksGraph = asks @"GraphReader"

