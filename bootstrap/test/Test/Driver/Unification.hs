module Test.Driver.Unification
  ( unifyingOfUnifiableTypesShouldNotGoWrong
  )
where

import Test.Tasty
import Test.Tasty.HUnit

import Driver.Transform
import Driver.Parser
import Driver.Unification

import Tlang.AST (typOperator, Name)
import Tlang.Parser
import Tlang.Graph.Core
import Tlang.Graph.Extension.Type

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec

-- | a predefined parser used to handle type expression during testing
parseType :: Monad m => Text -> m (Either (ParseErrorBundle Text Void) TypeAST, OperatorSpace)
parseType = driveParser ([] , typOperator) (pratt @(TypeLang Void _) @TypeAST eof Go) "Under Testing"

getType :: MonadFail m => Text -> m TypeAST
getType text = do
  (res, _) <- parseType text
  case res of
    Left err -> fail $ "Parser Error: " <> errorBundlePretty err
    Right t -> return t

buildAssertion :: Text -> Text -> Assertion
buildAssertion ia ib = do
  ta <- getType ia
  tb <- getType ib
  ((r1, g1 :: UnifyG), i) <- runToGraph mempty 1 ta
  ((r2, g2 :: UnifyG), j) <- runToGraph mempty i tb
  let root = hole (j + 1) (G 1)
      gr = overlays
           [ g1, g2
           , r1 -<< T (Bind Flexible 1 (Nothing @Name)) >>- root
           , r2 -<< T (Bind Flexible 2 (Nothing @Name)) >>- root
           ]
  res <- runDefaultUnify (gr :: UnifyG) r1 r2
  case res of
    Left err -> fail $ show err
    Right (r, g :: UnifyG) -> do
      (t :: TypeAST, _) <- runGraphType g [] ("@test", 1) r
      putStrLn $ show t
  return ()

buildCase :: (String, String) -> TestTree
buildCase v@(l, r) = testCase (show v) (buildAssertion (pack l) (pack r))

-- ** test cases

unifyingOfUnifiableTypesShouldNotGoWrong :: TestTree
unifyingOfUnifiableTypesShouldNotGoWrong = testGroup "Unifying of Unifiable Types Should Not Go Wrong" $
  buildCase <$>
  [ ("forall a. (a, a)", "forall a b. (a, b)")
  , ("forall f a. f a", "forall f a. a f")
  , ("12", "12")
  , ("forall a b c. (a, b, c)", "forall x y. (x, y, x)")
  , ("forall a b c. (a, b, c)", "forall x y z. (x, y, z)")
  ]
