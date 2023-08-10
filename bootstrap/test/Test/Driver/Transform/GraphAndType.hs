module Test.Driver.Transform.GraphAndType
  ( transformationShouldKeepTypeIntact
  )
where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Core (builtinStore)

import Driver.Transform

import Data.Text (Text, pack)
import Text.Megaparsec
import Compiler.SourceParsing

-- ** helpers

-- | a utility function used to help define test assertion
buildAssertion :: Text -> Assertion
buildAssertion text = do
  -- get parsed AST
  res'either <- getSurfaceType builtinStore "Under Testing" text
  typ <- case res'either of
    Left err -> fail $ "Parser Error: " <> errorBundlePretty err
    Right t -> return t

  -- convert type into graph
  ((root, g :: PlayG), _) <- runToGraph mempty 1 typ
  (typ2, _) <- runGraphType g [] ("@test", 1) root

  -- these two types should be equal
  typ @=? typ2

buildCase :: String -> TestTree
buildCase text = testCase text (buildAssertion $ pack text)


-- ** test cases

transformationShouldKeepTypeIntact :: TestTree
transformationShouldKeepTypeIntact = testGroup "Transformation Should Keep Type Content Intact" $
  buildCase <$>
  [ "forall a. a"
  , "forall a b c. c"
  , "forall a b c. <a, b, c>"
  , "forall a b c. b"
  , "forall a b c. a"
  , "forall a. {hello: a}"
  , "forall a. {hello: a, world: a}"
  , "forall a. (a, a)"
  , "forall a. <a, hello: a>"
  , "forall a b. (a, b)"
  -- TODO: add global declaration
  -- , "forall a b. a -> b"
  -- There is no way to distinguish `a` and `b`
  -- , "forall a (b = a) . b"
  , "forall a b. a b"
  , "forall a b. 1"
  , "forall a b. \"symbol\""
  , "forall a b. (a, {name: b, identity: <human, animal> })"
  , "forall a. a forall a. a"
  ]
