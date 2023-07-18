import Test.Tasty

import Test.Driver.Transform.GraphAndType (transformationShouldKeepTypeIntact)
import Test.Driver.Unification (unifyingOfUnifiableTypesShouldNotGoWrong)

main :: IO ()
main = defaultMain globalTests

globalTests :: TestTree
globalTests = testGroup "Global Test"
  [ testGroup "Transformation Test" [transformationShouldKeepTypeIntact]
  , testGroup "Graph Unification Test" [unifyingOfUnifiableTypesShouldNotGoWrong]
  ]
