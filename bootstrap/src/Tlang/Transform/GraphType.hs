{- | * Convert graphic representation of type back to its syntactic representation
-}

module Tlang.Transform.GraphType
  (

  -- ** transform Graphic Type back to Syntactic Type
    toSyntacticType

  -- ** relevant definitions
  , UnfoldGraphType (..)
  , GraphTypeConstrain
  )
where

import Tlang.AST
import Tlang.Graph.Core
import Tlang.Graph.Extension.Type
import Tlang.Extension
import Tlang.Generic ((:<:) (..), (:+:) (..))
import Tlang.Constraint (Prefix (..))

import Capability.Reader (HasReader, ask, asks, local)
import Control.Monad (forM)

import Data.Text (Text)
import Data.List (sortBy)

import qualified Data.Kind as Data (Type, Constraint)

type GraphTypeConstrain :: (Data.Type -> Data.Type) -> (Data.Type -> Data.Type)
                        -> (Data.Type -> Data.Type) -> (Data.Type -> Data.Type) -> Data.Type
                        -> (Data.Type -> Data.Type) -> (Data.Type -> Data.Type) -> Data.Type -> Data.Type
                        -> Data.Constraint
type family GraphTypeConstrain node m nodes edges info bind rep name a

class UnfoldGraphType node info | node -> info where
  unfoldGraphType :: (HasReader "graph" (CoreG nodes edges info) m, GraphTypeConstrain node m nodes edges info bind rep name a)
                  => (Hole nodes info -> m (Type bind rep name a))
                  -> node (Hole nodes info) -> info -> m (Type bind rep name a)

toSyntacticType
  :: ( GraphTypeConstrain nodes m nodes edges info bind rep name a
     , UnfoldGraphType nodes info
     , HasReader "graph" (CoreG nodes edges info) m
     )
  => Hole nodes info -> m (Type bind rep name a)
toSyntacticType (Hole n info) = unfoldGraphType toSyntacticType n info
{-# INLINE toSyntacticType #-}

-- ** Utilities
--
-- These are general code snippets

order :: Ord a => [(a, b)] -> [(a, b)]
order = sortBy \(a, _) (b, _) -> compare a b

-- | structure link from, sort by sub edge
sFrom :: (HasReader "graph" (CoreG nodes edges info) m, Eq (Hole nodes info), Ord (edges (Link edges)), T Sub :<: edges, Ord info, Ord (nodes (Hole nodes info)))
      => Hole nodes info -> m [(T Sub (Link edges), Hole nodes info)]
sFrom root = fmap order . asks @"graph" $ lFrom @(T Sub) (== root)
{-# INLINE sFrom #-}

-- | query whether one node has been translated into syntactic form, if so return the name
lookupName :: (Eq (Hole nodes info), HasReader "local" [(Hole nodes info, name)] m)
           => Hole nodes info -> m (Maybe name)
lookupName n = asks @"local" (lookup n)
{-# INLINE lookupName #-}

type WithLocalEnv m nodes edges info bind rep name a
  = (Eq (Hole nodes info), HasReader "local" [(Hole nodes info, a)] m)

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
  , T (Bind a) :<: edges
  , Ord (edges (Link edges)), Ord a
  , Eq (Hole nodes info), Eq a, Eq info
  , Forall (Prefix a) :<: bind
  , Functor rep
  , Ord info, Ord (nodes (Hole nodes info))
  )

-- | automatically handling binding edge
withBinding
  :: forall m nodes edges info bind rep name a. WithBindingEnv m nodes edges info bind rep name a
  => (Hole nodes info -> m (Type bind rep name a)) -> Hole nodes info -> m (Type bind rep name a) -> m (Type bind rep name a)
withBinding restore root m = do
  preBinds <- fmap order . asks @"graph" $ lTo @(T (Bind a)) (== root)
  scheme <- ask @"scheme"
  localBinds {- (flag, (node, name)) -} <- forM preBinds \(T (Bind flag _ name), a) -> fmap (flag,) $ (a,) <$> scheme name
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
           then return (New var)
           else return . Inc $ return var
  -- return the final type
  return $ foldr (\(name, bnd) bod -> TypBnd (inj $ Forall bnd) $ shift name bod) body bindings
{-# INLINE withBinding #-}

-- ** Instance definition
--
-- define logic here

type instance GraphTypeConstrain (x :+: y) m nodes edges info bind rep name a
  = ( GraphTypeConstrain x m nodes edges info bind rep name a
    , GraphTypeConstrain y m nodes edges info bind rep name a
    )
instance (UnfoldGraphType x info, UnfoldGraphType y info) => UnfoldGraphType (x :+: y) info where
  unfoldGraphType restore (Inl v) info = unfoldGraphType restore v info
  unfoldGraphType restore (Inr v) info = unfoldGraphType restore v info

type instance GraphTypeConstrain (T NodeTup) m nodes edges info bind rep name a
  = ( T Sub :<: edges
    , T NodeTup :<: nodes
    , Tuple :<: rep
    , WithBindingEnv m nodes edges info bind rep name a
    , Ord info, Ord (nodes (Hole nodes info))
    )
instance UnfoldGraphType (T NodeTup) Int where
  unfoldGraphType restore s@(T (NodeTup _)) info =
    let root = Hole (inj s) info in withLocal root $ withBinding restore root do
    subLinks <- asks @"graph" $ lFrom @(T Sub) (== root)
    Type . inj . Tuple <$> mapM restore (snd <$> subLinks)

type instance GraphTypeConstrain NodePht m nodes edges info bind rep name a
  = ( MonadFail m
    , WithBindingEnv m nodes edges info bind rep name a
    , WithLocalEnv m nodes edges info bind rep name a
    , NodePht :<: nodes
    , T Sub :<: edges
    , Ord info, Ord (nodes (Hole nodes info))
    )
instance UnfoldGraphType NodePht Int where
  unfoldGraphType restore NodePht info =
    let root = Hole (inj NodePht) info in withLocal root $ withBinding restore root do
    subLinks <- asks @"graph" $ lFrom @(T Sub) (== root)
    typs <- mapM restore (snd <$> subLinks)
    case typs of
      [typ] -> return typ
      _ -> fail "Illegal Phantom Node: it has no sub node or it has multiple sub nodes"

type instance GraphTypeConstrain (T (NodeRef name')) m nodes edges info bind rep name a
  = ( T (NodeRef name') :<: nodes
    , name' ~ a
    , WithLocalEnv m nodes edges info bind rep name a
    , WithBindingEnv m nodes edges info bind rep name a
    )
instance UnfoldGraphType (T (NodeRef a)) Int where
  unfoldGraphType restore s@(T (NodeRef _ name)) info =
    let root = Hole (inj s) info in withLocal root . withBinding restore root $ return (TypVar name)

type instance GraphTypeConstrain (T NodeBot) m nodes edges info bind rep name a
  = ( T NodeBot :<: nodes
    , WithLocalEnv m nodes edges info bind rep name a
    , WithBindingEnv m nodes edges info bind rep name a
    )
instance UnfoldGraphType (T NodeBot) Int where
  unfoldGraphType restore s@(T NodeBot) info =
    let root = Hole (inj s) info in withLocal root . withBinding restore root $ return TypPht


type instance GraphTypeConstrain (T (NodeLit Text)) m nodes edges info bind rep name a
  = ( T (NodeLit Text) :<: nodes
    , LiteralText :<: rep
    , WithLocalEnv m nodes edges info bind rep name a
    , WithBindingEnv m nodes edges info bind rep name a
    )
instance UnfoldGraphType (T (NodeLit Text)) Int where
  unfoldGraphType restore s@(T (NodeLit val)) info =
    let root = Hole (inj s) info in withLocal root . withBinding restore root $ return (Type . inj . LiteralText $ Literal val)


type instance GraphTypeConstrain (T (NodeLit Integer)) m nodes edges info bind rep name a
  = ( T (NodeLit Integer) :<: nodes
    , LiteralNatural :<: rep
    , WithLocalEnv m nodes edges info bind rep name a
    , WithBindingEnv m nodes edges info bind rep name a
    )
instance UnfoldGraphType (T (NodeLit Integer)) Int where
  unfoldGraphType restore s@(T (NodeLit val)) info =
    let root = Hole (inj s) info in withLocal root . withBinding restore root $ return (Type . inj . LiteralNatural $ Literal val)

type instance GraphTypeConstrain (T NodeApp) m nodes edges info bind rep name a
  = ( T NodeApp :<: nodes
    , WithLocalEnv m nodes edges info bind rep name a
    , WithBindingEnv m nodes edges info bind rep name a
    , MonadFail m
    , T Sub :<: edges
    )
instance UnfoldGraphType (T NodeApp) Int where
  unfoldGraphType restore s@(T (NodeApp _)) info =
    let root = Hole (inj s) info in withLocal root $ withBinding restore root do
    subLinks <- sFrom root
    case snd <$> subLinks of
      a:as -> TypCon <$> restore a <*> mapM restore as
      [] -> fail "Illegal Application Node: it has no sub nodes"

-- Record nodes with `Name` as label
type instance GraphTypeConstrain (T NodeRec) m nodes edges info bind rep name a
  = ( WithLocalEnv m nodes edges info bind rep name a
    , WithBindingEnv m nodes edges info bind rep name a
    , T (NodeHas Label) :<: nodes, T NodeRec :<: nodes
    , Record Label :<: rep
    , T Sub :<: edges
    , MonadFail m
    , Ord info, Ord (nodes (Hole nodes info))
    )
instance UnfoldGraphType (T NodeRec) Int where
  unfoldGraphType restore s@(T (NodeRec _)) info =
    let root = Hole (inj s) info in withLocal root $ withBinding restore root do
    subLinks <- sFrom root
    fields <- mapM (unfoldRecordLabel @Label restore) (snd <$> subLinks)
    return . Type . inj $ Record fields

type instance GraphTypeConstrain G m nodes edges info bind rep name a
  = MonadFail m
instance UnfoldGraphType G Int where
  unfoldGraphType _ (G _) _ = fail "Illegal Gen Node"

type instance GraphTypeConstrain Histo m nodes edges info bind rep name a
  = (MonadFail m)
instance UnfoldGraphType Histo Int where
  unfoldGraphType _ (Histo _) _ = fail "Illegal Histo Node"

type instance GraphTypeConstrain (T NodeSum) m nodes edges info bind rep name a
  = ( WithLocalEnv m nodes edges info bind rep name a
    , WithBindingEnv m nodes edges info bind rep name a
    , T Sub :<: edges
    , T (NodeHas Label) :<: nodes, T NodeSum :<: nodes
    , Variant Label :<: rep
    , MonadFail m
    , Ord info, Ord (nodes (Hole nodes info))
    )
instance UnfoldGraphType (T NodeSum) Int where
  unfoldGraphType restore s@(T (NodeSum _)) info =
    let root = Hole (inj s) info in withLocal root $ withBinding restore root do
    subLinks <- sFrom root
    fields <- mapM (unfoldVariantLabel @Label restore) (snd <$> subLinks)
    return . Type . inj $ Variant fields

type instance GraphTypeConstrain (T (NodeHas any)) m nodes edges info bind rep name a = ()
instance UnfoldGraphType (T (NodeHas any)) Int where
  unfoldGraphType _ (T (NodeHas _ _)) _ = error "impossible"

unfoldRecordLabel
  :: forall tag m nodes edges info bind rep name a
  . ( HasReader "graph" (CoreG nodes edges info) m
    , MonadFail m, Eq (Hole nodes info)
    , T Sub :<: edges, T (NodeHas tag) :<: nodes
    , Ord (edges (Link edges)), Ord info, Ord (nodes (Hole nodes info))
    )
  => (Hole nodes info -> m (Type bind rep name a)) -> Hole nodes info -> m (tag, Type bind rep name a)
unfoldRecordLabel restore root@(Hole n _) = do
  -- a label node can not be bound on, so no checking bindings here
  -- extract label
  tag <- case prj @(T (NodeHas tag)) n of
    Just (T (NodeHas _ tag)) -> return tag
    Nothing -> fail "Expect Tag Node but it is not"
  -- extract type node, a record label expect exact one type field
  fields <- asks @"graph" $ lFrom @(T Sub) (== root)
  case fields of
    [a] -> (tag,) <$> restore (snd a)
    _ -> fail "Illegal Tag Node: it has no type nodes but it is expected to have one"
{-# INLINE unfoldRecordLabel #-}

unfoldVariantLabel
  :: forall tag m nodes edges info bind rep name a
  . ( HasReader "graph" (CoreG nodes edges info) m
    , MonadFail m, Eq (Hole nodes info)
    , T Sub :<: edges, T (NodeHas tag) :<: nodes
    , Ord (edges (Link edges)), Ord info, Ord (nodes (Hole nodes info))
    )
  => (Hole nodes info -> m (Type bind rep name a)) -> Hole nodes info -> m (tag, Maybe (Type bind rep name a))
unfoldVariantLabel restore root@(Hole n _) = do
  -- a label node can not be bound on, so no checking bindings here
  -- extract label
  tag <- case prj @(T (NodeHas tag)) n of
    Just (T (NodeHas _ tag)) -> return tag
    Nothing -> fail "Expect Tag Node but it is not"
  -- extract type node, a record label expect exact one type field
  fields <- asks @"graph" $ lFrom @(T Sub) (== root)
  case fields of
    [a] -> (tag,) . Just <$> restore (snd a)
    [] -> return (tag, Nothing)
    _ -> fail "Illegal Tag Node: it has more than one type nodes but it is expected to have exactly one or have none"
{-# INLINE unfoldVariantLabel #-}
