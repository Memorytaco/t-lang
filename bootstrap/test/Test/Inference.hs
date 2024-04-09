module Test.Inference
  ( inferringExpressionWithNoError
  , inferringExpressionWithError
  )
where


import Compiler.SourceParsing
import Compiler.TypeChecking
import Language.Core
import Data.Text
import Text.Megaparsec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, testCaseSteps, assertFailure, assertBool)
import Prettyprinter
import Transform.Desugar (pruneForallType)

buildCorrectCase :: Text -> TestTree
buildCorrectCase text = testCaseSteps (unpack text) \output -> do
  res'either <- getSurfaceExpr builtinStore "Under Testing" text
  e <- case res'either of
    Left err -> assertFailure $ "Parser Error: " <> errorBundlePretty err
    Right e -> return e
  tcExprToSyntacticType [] 0 ("t", 0) e >>= \case
    Right (t, _) -> output $ show $ pretty $ pruneForallType t
    Left err -> assertFailure $ "Inference Error: " <> show err

buildWrongCase :: Text -> TestTree
buildWrongCase text = testCase (unpack text) do
  res'either <- getSurfaceExpr builtinStore "Under Testing" text
  e <- case res'either of
    Left err -> fail $ "Parser Error: " <> errorBundlePretty err
    Right e -> return e
  tcExprToSyntacticType [] 0 ("t", 0) e >>= \case
    Right (_, _) -> assertFailure "Inference Should not succeed"
    Left err -> assertBool (show err) True

inferringExpressionWithNoError :: TestTree
inferringExpressionWithNoError = testGroup "Inferring Expression With No Error" $
  buildCorrectCase <$>
  [ "9"
  , "9.0"
  , "(9)"
  , "(9, 9.0)"
  , "(9, \"text\")"
  , "(1, 2)"
  , "(1, 2, 3)"
  , "(1, 2, 3, 4)"
  , "(1, 2, 3, 4, 5)"
  , "(1, 2, 3, 4, 5, 6)"
  , "(1, 2, 3, 4, 5, 6, 7)"
  , "(1, 2, 3, 4, 5, 6, 7, 8)"
  , "(1, 2, 3, 4, 5, 6, 7, 8, 9)"
  , "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)"
  , "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)"
  , "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)"
  , "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)"
  , "let ?a = 3 in a"
  , "let ?a = 3 in 3"
  , "[ ?a = a ]"
  ]

inferringExpressionWithError :: TestTree
inferringExpressionWithError = testGroup "Inferring Expression With Error" $
  buildWrongCase <$>
  [ "let ( ?a, ?b) = 3 in a"
  , "let ( ?a, ?b) = (3,4,5) in a"
  , "let ?a = 3 in a 3"
  , "1 2"
  ]
