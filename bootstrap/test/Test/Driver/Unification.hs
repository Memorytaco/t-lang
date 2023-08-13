module Test.Driver.Unification
  ( unifyingOfUnifiableTypesShouldNotGoWrong
  )
where

import Test.Tasty
import Test.Tasty.HUnit

import Driver.Transform
import Driver.Unification

import Language.Core (builtinStore, Name, TypSurface)
import Tlang.Graph.Core
import Tlang.Graph.Extension.GraphicType

import Data.Text (Text, pack)
import Text.Megaparsec

import Compiler.SourceParsing

assertType :: MonadFail m => Text -> m TypSurface
assertType text = do
  res'either <- getSurfaceType builtinStore "Under Testing" text
  case res'either of
    Left err -> fail $ "Parser Error: " <> errorBundlePretty err
    Right t -> return t

buildAssertion :: Text -> Text -> Assertion
buildAssertion ia ib = do
  ta <- assertType ia
  tb <- assertType ib
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
      (_ :: TypSurface, _) <- runGraphType g [] ("@test", 1) r
      return ()
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
  , ("forall a b. (b, b)", "forall a. (a, a)")
  -- , ("forall a b (c ~ (a, b)) . (a, c, b)", "forall x y z. (x, y, z)")
  -- , ("forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c", "forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c")
  ]
