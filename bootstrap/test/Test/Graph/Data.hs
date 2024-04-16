module Test.Graph.Data
  ( test'toEdges
  , test'removeEdge
  , test'einduce
  , test'GraphData
  )
where

import Graph.Data
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (arbitrary, Gen, forAll, Property)


type TestGraph = Graph Int Int


case'toEdges :: [(String, ([(Int, Int, Int)], TestGraph))]
case'toEdges =
  [ ( "none" , (
    []
  , pure 0
  ))
  , ( "none with overlay" , (
    []
  , Overlay (pure 0) (pure 1)
  ))
  , ( "1 -> 1", (
    [(0, 1, 1)]
  , Connect 0 (pure 1) (pure 1)
  ))
  , ( "1 -> 1, 1 -> 1", (
    [(0, 1, 1), (0, 1, 1)]
  , let a = Connect 0 (pure 1) (pure 1)
    in Overlay a a
  ))
  , ( "1 -> 2, 2 -> 3, 1 -> 3", (
    [(0, 1, 2), (0, 1, 3), (0, 2, 3)]
  , Connect 0 (pure 1) $ Connect 0 (pure 2) (pure 3)
  ))
  , ( "1 ->0 2, 1 ->0 3, 2 ->1 3", (
    [(0, 1, 2), (0, 1, 3), (1, 2, 3)]
  , Connect 0 (pure 1) $ Connect 1 (pure 2) (pure 3)
  ))
  , ( "1, 2 ->1 3", (
    [(1, 2, 3)]
  , Overlay (pure 1) $ Connect 1 (pure 2) (pure 3)
  ))
  , ( "5, 2 ->1 3", (
    [(1, 2, 3)]
  , Overlay (pure 5) $ Connect 1 (pure 2) (pure 3)
  ))
  , ( "none with multiple overlays", (
    []
  , Overlay (pure 3) (Overlay (pure 4) (pure 5))
  ))
  ]

test'toEdges :: TestTree
test'toEdges = testGroup "toEdges" $ fmap load case'toEdges
  where load (name, (left, right)) = testCase name do
          assertEqual name left (toEdges right)


case'removeEdge :: [(String, ([Int], [(Int, Int, Int)], Int -> Int -> Int -> Bool, TestGraph))]
case'removeEdge =
  [ ( "remove nothing", (
    [1,2,3], [(0, 1, 2), (0, 1, 3), (1, 2, 3)]
  , \_ _ _ -> False
  , graph1
  ))
  , ( "remove everything", (
    [1,2,3], []
  , \_ _ _ -> True
  , graph1
  ))
  , ( "remove one edge 0", (
    [1,2,3], [(0, 1, 3), (1, 2, 3)]
  , \e _ b -> e == 0 && b == 2
  , graph1
  ))
  , ( "remove one edge 1", (
    [1,2,3], [(0, 1, 2), (0, 1, 3)]
  , \e _ _ -> e == 1
  , graph1
  ))
  , ( "remove everything with overlay", (
    [1,2,3], []
  , \_ _ _ -> True
  , graph2
  ))
  , ( "remove one edge with overlay 0", (
    [1,2,3], [(0, 1, 2), (0, 1, 3)]
  , \e _ _ -> e == 1
  , graph2
  ))
  , ( "remove one edge with overlay 1", (
    [1,2,3], [(0, 1, 3), (1, 2, 3)]
  , \e _ b -> e == 0 && b == 2
  , graph2
  ))
  , ( "remove every single edge, connect", (
    [1,2,3,4], []
  , \_ _ _ -> True
  , graph3
  ))
  , ( "remove edge, complex", (
    [1,2,3,4,5], [(0, 1, 2), (0, 1, 3), (1, 2, 3)]
  , \e _ _ -> e `elem` [3, 4]
  , graph4
  ))
  , ( "remove all with loop edge, simple", (
    [1,2,3], []
  , \_ _ _ -> True
  , graph5
  ))
  , ( "remove some with loop edge, simple", (
    [1,2,3], [(1, 2, 3)]
  , \e _ _ -> e == 0
  , graph5
  ))
  , ( "remove loop edge, simple", (
    [1,2,3], [(0, 1, 2), (0, 1, 3), (1, 2, 3)]
  , \_ a b -> a == b
  , graph5
  ))
  , ( "remove edge with connection", (
    [1,2], [(0, 2, 2), (1, 1, 2)]
  , \e a b -> e == 0 && a == 1 && b == 2
  , graph6
  ))
  ]
  where graph1 = Connect 0 (pure 1) $ Connect 1 (pure 2) (pure 3)
        graph2 = Overlay graph1 graph1
        graph3 = Connect 3 (pure 4) graph2
        graph4 = Connect 4 graph3 (pure 5)
        graph5 = Connect 0 (pure 1) graph2
        graph6 = Connect 0 (Connect 1 (pure 1) (pure 2)) (pure 2)

test'removeEdge :: TestTree
test'removeEdge = testGroup "removeEdge" $ load <$> case'removeEdge
  where load (name, (vs, es, p, g)) = testCase name do
          let gr = removeEdge p g
          assertEqual "vertices" vs (order $ toVertices gr)
          assertEqual "edges" es (order $ toEdges gr)

case'einduce :: [(String, TestGraph -> Bool, TestGraph)]
case'einduce =
  [ ( "remove one edge", notElem (0, 1, 1) . operate \e a b ->
      if (e == 0) && (a == b) && (b == 1) then Nothing else Just (e, a, b)
    , Connect 0 (pure 1) $ Overlay (pure 1) (pure 2)
  )
  , ( "replace edge only over overlay", ([(1, 1, 1), (1, 1, 2)] ==) . operate \_ a b ->
      Just (1, a, b)
    , Connect 0 (pure 1) $ Overlay (pure 1) (pure 2)
  )
  , ( "replace edge only over connect", ([(0, 1, 2), (0, 1, 3), (0, 2, 3)] ==) . operate \e a b ->
      Just (if e == 1 then 0 else e, a, b)
    , Connect 0 (pure 1) $ Connect 1 (pure 2) (pure 3)
  )
  , ( "replace edge between two vertex", ([(1, 1, 3), (1, 1, 4), (1, 2, 3)] ==) . operate \e a b ->
      if e == 0 && a == 1 then Just (1, a, b + 1) else Just (e, a, b)
    , Connect 0 (pure 1) $ Connect 1 (pure 2) (pure 3))
  , ( "switch vertex and remove others", ([(1, 3, 1), (1, 4, 1)] ==) . operate \e a b ->
      if e == 0 && a == 1 then Just (1, b + 1, a) else Nothing
    , Connect 0 (pure 1) $ Connect 1 (pure 2) (pure 3))
  ]
  where operate f = order . toEdges . einduce f

case'outFrom :: [(String, ([Int], [(Int, Int)]), TestGraph)]
case'outFrom =
  [ ( "simple case, 2 edges", ([1], [(0, 2), (0, 3)]), g1 )
  , ( "simple case, 1 edge", ([2], [(1, 3)]), g1 )
  , ( "simple case, 2 edges, overlay", ([1], [(0, 2), (0, 3)]), g2 )
  , ( "simple case, 1 edge, overlay", ([2], [(1, 3)]), g2 )
  , ( "simple case, 2 edges, connect 0", ([1], [(0, 2), (0, 3)]), g3 )
  , ( "simple case, 2 edges, connect 0", ([2], [(1, 3)]), g3 )
  ]
  where
    -- simple tests
    g1 = Connect 0 (pure 1) $ Connect 1 (pure 2) (pure 3)
    -- duplication should not matter
    g2 = Overlay g1 g1
    -- other edges should not interfere with current setting
    g3 = Connect 0 (pure 0) g2

test'outFrom :: TestTree
test'outFrom = testGroup "outFrom" $ load <$> case'outFrom
  where
    load (name, (n, res), g) = testCase name do
      assertBool name $ order (snd <$> outFrom (`elem` n) g) == res

case'inTo :: [(String, ([Int], [(Int, Int)]), TestGraph)]
case'inTo =
  [ ( "simple case, 2 edges", ([3], [(1, 0), (2, 1)]), g1 )
  , ( "simple case, 1 edge", ([2], [(1, 0)]), g1 )
  , ( "simple case, 2 edges, overlay", ([3], [(1, 0), (2, 1)]), g2 )
  , ( "simple case, 1 edge, overlay", ([2], [(1, 0)]), g2 )
  , ( "simple case, 2 edges, connect 0", ([3], [(0, 0), (1, 0), (2, 1)]), g3 )
  , ( "simple case, 2 edges, connect 0", ([2], [(0, 0), (1, 0)]), g3 )
  ]
  where
    -- simple tests
    g1 = Connect 0 (pure 1) $ Connect 1 (pure 2) (pure 3)
    -- duplication should not matter
    g2 = Overlay g1 g1
    -- other edges should not interfere with current setting
    g3 = Connect 0 (pure 0) g2

test'inTo :: TestTree
test'inTo = testGroup "inTo" $ load <$> case'inTo
  where load (name, (n, res), g) = testCase name do
          assertBool name $ order (fst <$> inTo (`elem` n) g) == res

-- | collapse should be equal to a combination of 'removeEdge' and 'prune'
prop'collapse :: Property
prop'collapse = forAll (arbitrary :: Gen TestGraph) \gr ->
  collapse gr == collapse' gr
    where collapse' = prune . removeEdge \_ _ _ -> True

-- | see `decompose` for property description.
prop'decompose :: Property
prop'decompose = forAll (arbitrary :: Gen TestGraph) (verify . decompose)
  where
    verify (Connect _ None _) = True
    verify (Connect _ (Vertex _) _) = True
    verify (Connect _ _ None) = True
    verify (Connect _ _ (Vertex _)) = True
    verify (Connect {}) = False
    verify _ = True

test'einduce :: TestTree
test'einduce = testGroup "einduce" $ load <$> case'einduce
  where load (name, predicate, graph) = testCase name do
          assertBool name (predicate graph)

test'GraphData :: TestTree
test'GraphData = testGroup "Graph.Data"
  [ test'toEdges
  , test'removeEdge
  , test'einduce
  , test'outFrom
  , test'inTo
  , testProperty "collapse == prune . removeEdge \\_ _ -> True" prop'collapse
  , testProperty "decompose will leave graph in non-Connect compound form" prop'decompose
  ]
