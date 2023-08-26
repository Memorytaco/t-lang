{- | * Convert graphic representation of type back to its syntactic representation
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Transform.GraphType
  (

  -- ** core builder
    Conversion
  , runConversion
  , treeConversion

  -- ** error definition
  , GraphToTypeErr (..)
  , HasGraphToTypeErr
  , GraphTypeProperty (..)

  -- ** building blocks
  , literal01
  , literal02
  , literal03
  , literal04
  , literal05
  , literal06

  , structure01

  , special01
  , special02
  )
where

import Language.Core
import Language.Core.Extension

import Graph.Core
import Graph.Extension.GraphicType
import Language.Generic ((:<:), inj, prj, Recursion (..))

import Capability.Reader (HasReader, ask, asks, local)
import Control.Monad (forM)

import Data.Function (fix)
import Data.String (IsString (..))

import Capability.Error

-- | Conversion error
data GraphToTypeErr node
  = GTFailUnmatch node
  | GTFailUnexpect String
  | GTFailProperty (GraphTypeProperty node)
  deriving (Show, Eq)

data GraphTypeProperty node
  = PropFailSatisfy node String
  deriving (Show, Eq)

type HasGraphToTypeErr node m
  = ( HasThrow GraphToTypeErr (GraphToTypeErr node) m
    , HasCatch GraphToTypeErr (GraphToTypeErr node) m
    )

unmatch :: HasGraphToTypeErr node m => node -> m a
unmatch = throw @GraphToTypeErr . GTFailUnmatch

unexpect :: HasGraphToTypeErr node m => String -> m a
unexpect = throw @GraphToTypeErr . GTFailUnexpect

failProp :: HasGraphToTypeErr node m => GraphTypeProperty node -> m a
failProp = throw @GraphToTypeErr . GTFailProperty

data Conversion nodes edges info bind rep name m a
  = HasReader "graph" (CoreG nodes edges info) m
  => Conversion (Recursion m (Hole nodes info) (Type bind rep name a))

-- | drive a conversion from graph to type
runConversion
  :: Conversion nodes edges info bind rep name m a
  -> Hole nodes info -> m (Type bind rep name a)
runConversion (Conversion (Recursion f)) = fix f

-- | build a tree like conversion
treeConversion
  :: (HasGraphToTypeErr (Hole nodes info) m, HasEqNode nodes info, HasReader "graph" (CoreG nodes edges info) m)
  => [Conversion nodes edges info bind rep name m a] -> Conversion nodes edges info bind rep name m a
treeConversion = foldr foldT (Conversion $ Recursion \_ n -> unmatch n)
  where
    foldT (Conversion (Recursion f)) (Conversion (Recursion g)) = Conversion $ Recursion \restore n ->
      catch @GraphToTypeErr (f restore n) $ \case
        e@(GTFailUnmatch n') ->
          if n' == n
            then g restore n
            else throw @GraphToTypeErr e
        e -> throw @GraphToTypeErr e


-- ** Utilities
--
-- These are general code snippets

-- | structure link from, sort by sub edge
sFrom :: (HasReader "graph" (CoreG nodes edges info) m, Eq (Hole nodes info), Ord (edges (Link edges)), T Sub :<: edges, Ord info, Ord (nodes (Hole nodes info)))
      => Hole nodes info -> m [(T Sub (Link edges), Hole nodes info)]
sFrom root = asks @"graph" $ lFrom @(T Sub) (== root)

-- | query whether one node has been translated into syntactic form, if so return the name
lookupName :: (Eq (Hole nodes info), HasReader "local" [(Hole nodes info, name)] m)
           => Hole nodes info -> m (Maybe name)
lookupName n = asks @"local" (lookup n)

-- | if the node has already been translated, return `TypVar name`, and otherwise pass
-- control to the following monad block
withLocal :: (Eq (Hole nodes info), HasReader "local" [(Hole nodes info, a)] m)
          => Hole nodes info -> m (Type bind rep name a) -> m (Type bind rep name a)
withLocal root m = lookupName root >>= \case
  Just name -> return (TypVar name)
  Nothing -> m
{-# INLINE withLocal #-}

-- | environment for `WithBinding`
type WithBindingEnv m nodes edges info bind rep name a =
  ( name ~ a
  , HasReader "graph" (CoreG nodes edges info) m
  , HasReader "local" [(Hole nodes info, a)] m
  , HasReader "scheme" (Maybe a -> m a) m
  , T (Binding a) :<: edges
  , Ord a , Eq a
  , Forall (Prefix a) :<: bind
  , Functor rep, Functor bind
  , HasOrderGraph nodes edges info
  )

-- | automatically handling binding edge
withBinding
  :: forall m nodes edges info bind rep name a. (WithBindingEnv m nodes edges info bind rep name a)
  => (Hole nodes info -> m (Type bind rep name a)) -> Hole nodes info -> m (Type bind rep name a) -> m (Type bind rep name a)
withBinding restore root m = do
  preBinds <- asks @"graph" $ lTo @(T (Binding a)) (== root)
  scheme <- ask @"scheme"
  localBinds {- (flag, (node, name)) -} <- forM preBinds \(T (Binding flag _ name), a) -> fmap (flag,) $ (a,) <$> scheme name
  body <- local @"local" ((snd <$> localBinds) <>) m
  bindings <- forM localBinds \(flag, (node, name)) -> do
    typ <- restore node
    case flag of
      Rigid -> return (name, name :~ typ)
      Flexible -> return (name, name :> typ)
      Explicit -> return (name, name :> typ)
  let shift val term = do
        var <- term
        if var == val
           then return (Bind var)
           else return . Free $ return var
  -- return the final type
  return $ foldr (\(name, bnd) bod -> TypBnd (inj $ Forall bnd) $ shift name bod) body bindings


-- | RULE: T NodeTup
literal01
  :: ( HasGraphToTypeErr (Hole nodes info) m, T NodeTup :<: nodes, Tuple :<: rep
     , WithBindingEnv m nodes edges info bind rep name a
     , T Sub :<: edges
     )
  => Conversion nodes edges info bind rep name m a
literal01 = Conversion $ Recursion \restore -> maybeHole unmatch \root (T (NodeTup _)) _ ->
  withLocal root $ withBinding restore root do
    subLinks <- asks @"graph" $ lFrom @(T Sub) (== root)
    Type . inj . Tuple <$> mapM restore (snd <$> subLinks)

-- | RULE: T (NodeRef name)
literal02
  :: ( T (NodeRef name) :<: nodes
     , HasGraphToTypeErr (Hole nodes info) m
     , WithBindingEnv m nodes edges info bind rep name a
     )
  => Conversion nodes edges info bind rep name m a
literal02 = Conversion $ Recursion \restore -> maybeHole unmatch \root (T (NodeRef _ (name :: name))) _ ->
  withLocal root $ withBinding restore root do
    return (TypVar name)

-- | RULE: T NodeArr
literal03
  :: ( HasGraphToTypeErr (Hole nodes info) m
     , T NodeArr :<: nodes
     , WithBindingEnv m nodes edges info bind rep name a
     , IsString a
     )
  => Conversion nodes edges info bind rep name m a
literal03 = Conversion $ Recursion \restore -> maybeHole unmatch \root (T NodeArr) _ ->
  withLocal root $ withBinding restore root do
    return (TypVar $ fromString "->")

-- | RULE: T (NodeLit @lit)
literal04
  :: forall lit nodes info rep m edges a bind name
   . ( HasGraphToTypeErr (Hole nodes info) m
     , T (NodeLit lit) :<: nodes
     , Literal lit :<: rep
     , WithBindingEnv m nodes edges info bind rep name a
     )
  => Conversion nodes edges info bind rep name m a
literal04 = Conversion $ Recursion \restore -> maybeHole @(T (NodeLit lit)) unmatch \root (T (NodeLit lit)) _ ->
  withLocal root $ withBinding restore root do
    return (Type . inj $ Literal lit)

-- | RULE: T NodeRec
literal05
  :: forall label nodes info rep m edges a bind name
   . ( HasGraphToTypeErr (Hole nodes info) m
     , T (NodeHas label) :<: nodes
     , T NodeRec :<: nodes
     , Record label :<: rep
     , WithBindingEnv m nodes edges info bind rep name a
     , T Sub :<: edges
     )
  => Conversion nodes edges info bind rep name m a
literal05 = Conversion $ Recursion \restore -> maybeHole  unmatch \root (T (NodeRec _)) _ ->
  withLocal root $ withBinding restore root do
    subLinks <- sFrom root
    fields <- mapM (unfoldLabel @label restore) (snd <$> subLinks) >>= mapM \case
      (label, Just typ) -> return (label, typ)
      (_, Nothing) -> failProp $ PropFailSatisfy root
        "Illegal Tag Node under NodeRec: it has no type nodes but it is expected to have one"
    return . Type . inj $ Record fields

-- | RULE: T NodeSum
literal06
  :: forall label nodes info rep m edges a bind name
   . ( HasGraphToTypeErr (Hole nodes info) m
     , T (NodeHas label) :<: nodes
     , T NodeSum :<: nodes
     , Variant label :<: rep
     , WithBindingEnv m nodes edges info bind rep name a
     , T Sub :<: edges
     )
  => Conversion nodes edges info bind rep name m a
literal06 = Conversion $ Recursion \restore -> maybeHole  unmatch \root (T (NodeSum _)) _ ->
  withLocal root $ withBinding restore root do
    subLinks <- sFrom root
    fields <- mapM (unfoldLabel @label restore) (snd <$> subLinks)
    return . Type . inj $ Variant fields

-- | RULE: T NodeApp
structure01
  :: ( HasGraphToTypeErr (Hole nodes info) m
     , T NodeApp :<: nodes
     , WithBindingEnv m nodes edges info bind rep name a
     , T Sub :<: edges
     )
  => Conversion nodes edges info bind rep name m a
structure01 = Conversion $ Recursion \restore -> maybeHole unmatch \root (T (NodeApp _)) _ ->
  withLocal root $ withBinding restore root do
    subLinks <- sFrom root
    case snd <$> subLinks of
      a:as -> TypCon <$> restore a <*> mapM restore as
      [] -> failProp $ PropFailSatisfy root "Illegal Application Node: it has no sub nodes"


-- | RULE: NodePht
special01
 :: ( HasGraphToTypeErr (Hole nodes info) m, NodePht :<: nodes
    , WithBindingEnv m nodes edges info bind rep name a
    , T Sub :<: edges
    )
  => Conversion nodes edges info bind rep name m a
special01 = Conversion $ Recursion \restore -> maybeHole unmatch \root NodePht _ ->
  withLocal root $ withBinding restore root do
    subLinks <- asks @"graph" $ lFrom @(T Sub) (== root)
    typs <- mapM restore (snd <$> subLinks)
    case typs of
      [typ] -> return typ
      _ -> failProp $ PropFailSatisfy root "Illegal Phantom Node: it has no sub node or it has multiple sub nodes"

-- | RULE: T NodeBot
special02
 :: ( HasGraphToTypeErr (Hole nodes info) m
    , T NodeBot :<: nodes
    , WithBindingEnv m nodes edges info bind rep name a
    )
  => Conversion nodes edges info bind rep name m a
special02 = Conversion $ Recursion \restore -> maybeHole unmatch \root (T NodeBot) _ ->
  withLocal root $ withBinding restore root do
    return TypPht

-- | a helper function to unfold `NodeHas` node into pairs of label and type
unfoldLabel
  :: forall label m nodes edges info bind rep name a
  . ( HasReader "graph" (CoreG nodes edges info) m
    , Eq (Hole nodes info)
    , T Sub :<: edges
    , T (NodeHas label) :<: nodes
    , HasOrderGraph nodes edges info
    , HasGraphToTypeErr (Hole nodes info) m
    )
  => (Hole nodes info -> m (Type bind rep name a)) -> Hole nodes info
  -> m (label, Maybe (Type bind rep name a))
unfoldLabel restore root@(Hole tag _) = do
  -- a label node can not be bound on, so no not checking binding edges here
  label <- case prj @(T (NodeHas label)) tag of
    Just (T (NodeHas _ label)) -> return label
    Nothing -> unexpect "Expect Tag Node but it is not"
  -- extract type node, a label may or may not have a type field
  fields <- asks @"graph" $ lFrom @(T Sub) (== root)
  case fields of
    [snd -> n] -> (label,) . Just <$> restore n
    [] -> return (label, Nothing)
    _ -> failProp $ PropFailSatisfy root "Illegal Tag Node: it has more than one type nodes but it is expected to have exactly one or none"
