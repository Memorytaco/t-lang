module Test.Driver.Transform.GraphAndType
  ( transformationShouldKeepTypeIntact
  )
where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Core (builtinStore, GraphicTypeSurface)

import Driver.Transform

import Data.Text (Text, pack)
import Text.Megaparsec
import Compiler.SourceParsing

-- ** helpers

-- | a utility function used to help define test assertion
buildAssertion :: Text -> (String -> IO ()) -> Assertion
buildAssertion text output = do
  -- get parsed AST
  res'either <- getSurfaceType builtinStore "Under Testing" text
  typ <- case res'either of
    Left err -> fail $ "Parser Error: " <> errorBundlePretty err
    Right t -> return t

  -- convert type into graph
  ((root, g :: GraphicTypeSurface), _) <- toGraphicTypeMock mempty 1 typ >>= \case
    Left err -> fail $ show err
    Right v -> return v
  output $ "root node :" <> show root
  typ2 <- runGraphType g [] ("@test", 1) syntacticType root >>= \case
    Left err -> fail $ show err
    Right (typ2, _) -> return typ2
  output $ "type(origin): " <> show typ
  output $ "type(back): " <> show typ2

  -- these two types should be equal
  typ @=? typ2

buildCase :: String -> TestTree
buildCase text = testCaseSteps text (buildAssertion $ pack text)


-- ** test cases

transformationShouldKeepTypeIntact :: TestTree
transformationShouldKeepTypeIntact = testGroup "Transformation Should Keep Type Content Intact" $
  buildCase <$>
  [ "forall a. a"
  -- , "forall a b c. c"
  -- , "forall b a. a"
  , "forall b (a ~ (b, b)) . (b, a)"
  , "forall b (a = (b, b)) . (b, a)"
  , "forall b (a ~ (b, b)) . a"
  , "forall b (a = (b, b)) . a"
  , "forall a b c. <a, b, c>"
  -- , "forall a b c. b"
  -- , "forall a b c. a"
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
  , "forall a (b ~ (a, a)) . (a, b, a)"
  , "forall a b. \"symbol\""
  , "forall a b. (a, {name: b, identity: <human, animal> })"
  , "forall a. a forall a. a"
  ]
