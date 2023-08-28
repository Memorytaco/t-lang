import Test.Tasty

import Test.Driver.Transform.GraphAndType (transformationShouldKeepTypeIntact)
import Test.Driver.Unification (unifyingOfUnifiableTypesShouldNotGoWrong)
import Test.Inference (inferringExpressionWithNoError, inferringExpressionWithError)

main :: IO ()
main = defaultMain globalTests

globalTests :: TestTree
globalTests = testGroup "Global Test"
  [ testGroup "Transformation Test" [transformationShouldKeepTypeIntact]
  , testGroup "Graph Unification Test" [unifyingOfUnifiableTypesShouldNotGoWrong]
  , testGroup "Inference Test" [inferringExpressionWithNoError, inferringExpressionWithError]
  ]
