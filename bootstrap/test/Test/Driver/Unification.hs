module Test.Driver.Unification
  ( unifyingOfUnifiableTypesShouldNotGoWrong
  )
where

import Test.Tasty
import Test.Tasty.HUnit

import Driver.Transform
import Driver.Transform.Desugar (addBindingToType)
import Driver.Unification

import Language.Core (builtinStore, Name, TypSurface)
import Graph.Core
import Graph.Extension.GraphicType
import Language.Generic ((:+:))
import Prettyprinter

import Data.Text (Text, pack)
import Text.Megaparsec

import Compiler.SourceParsing

type CustomG = CoreG (G :+: Histo :+: SurfaceGNodes) (Pht O :+: SurfaceGEdges) Int

assertType :: MonadFail m => Text -> m TypSurface
assertType text = do
  res'either <- getSurfaceType builtinStore "Under Testing" text
  case res'either of
    Left err -> fail $ "Parser Error: " <> errorBundlePretty err
    Right t -> return t

buildAssertionText :: Text -> Text -> (String -> IO ()) -> Assertion
buildAssertionText ia ib output = do
  ta <- assertType ia
  tb <- assertType ib
  ((r1, g1 :: CustomG), i) <- toGraphicTypeMock mempty 1 ta >>= \case
    Right ((root, gr), i) -> (, i) . (root, ) <$> (snd <$> addBindingToType gr root)
    Left err -> fail $ show err
  ((r2, g2), j) <- toGraphicTypeMock mempty i tb >>= \case
    Right ((root, gr), j) -> (, j) . (root, ) <$> (snd <$> addBindingToType gr root)
    Left err -> fail $ show err
  let root = hole (G 1) (j + 1)
      gr = overlays
           [ g1, g2
           , r1 -<< T (Binding Flexible 1 (Nothing @Name)) >>- root
           , r2 -<< T (Binding Flexible 1 (Nothing @Name)) >>- root
           ]
  unify gr r1 r2 >>= \case
    Left err -> fail $ show err
    Right (r, g) ->
      runGraphType g [] ("@", 1) syntacticType r >>= \case
      Left err -> fail $ show err
      Right (t :: TypSurface, _) -> output $ show (pretty t)

buildCase :: (String, String) -> TestTree
buildCase v@(l, r) = testCaseSteps (show v) $ buildAssertionText (pack l) (pack r)

-- ** test cases

unifyingOfUnifiableTypesShouldNotGoWrong :: TestTree
unifyingOfUnifiableTypesShouldNotGoWrong = testGroup "Unifying of Unifiable Types Should Not Go Wrong" $
  buildCase <$>
  [ ("forall a. (a, a)", "forall a b. (a, b)")
  , ("forall f a. f a", "forall f a. a f")
  , ("forall a. a", "forall b. b")
  , ("12", "12")
  , ("forall a b c. (a, b, c)", "forall x y. (x, y, x)")
  , ("forall a b c. (a, b, c)", "forall x y z. (x, y, z)")
  , ("forall a b. (b, b)", "forall a. (a, a)")
  , ("forall a b (c ~ (a, b)) . (a, c, b)", "forall x y z. (x, y, z)")
  , ("forall a b. a -> b", "forall a. a -> a")
  , ("forall a b. a -> int", "forall a. a -> a")
  , ("forall a b. a -> int", "forall a. float -> a")
  , ("forall a b. (a, (b, b))", "forall a b. (a, a)")

  -- , ("forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c"
  --   , "forall b (a = b -> b) (c ~ forall (x = forall x. x -> x) b. (x -> x) -> (b -> b) ) . a -> c"
  --   )
  , ("forall x. maybe x", "forall f y. maybe (f y)")
  ]
