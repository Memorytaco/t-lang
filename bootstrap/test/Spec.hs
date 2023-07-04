import Test.Tasty
import System.Environment

import Test.Driver.Transform.GraphAndType (transformationShouldKeepTypeIntact)

main :: IO ()
main = defaultMain globalTests

globalTests :: TestTree
globalTests = testGroup "Global Test"
  [ testGroup "Transformation Test" [transformationShouldKeepTypeIntact]
  ]
