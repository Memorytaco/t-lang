{-# LANGUAGE TypeFamilyDependencies #-}

module Tlang.Inference.Expr
  (
    ConstraintGen
  , stageConstraint
  , StageConstraint (..)

  , ConstraintGenErr (..)
  , failConstraintMsg

  , PatternData (..)

  , genConstraint
  , genPatternConstraint
  )
where

import Tlang.AST hiding (Type)
import Tlang.Extension.Decl
import Tlang.Extension
import Tlang.Graph.Core
import Tlang.Unification.Type
import Tlang.Graph.Extension.Type
import Tlang.Generic ((:<:) (..), (:+:) (..), type (|:) (..), Base)

import Capability.State (HasState, get, modify)
import Capability.Reader (HasReader, asks)
import Capability.Error (HasThrow, throw)
import Control.Monad (forM, foldM)
import Data.Functor.Foldable (cata)
import Data.Functor ((<&>), ($>))

import Data.Kind (Constraint, Type)
import Data.Text (Text)

data ConstraintGenErr
   = FailGenMsg String
   deriving (Show, Eq, Ord)

failConstraintMsg :: HasThrow "fail" ConstraintGenErr m => String -> m a
failConstraintMsg = throw @"fail" . FailGenMsg

-- | define dependency between constraints
data ConstraintOrder
newtype NodeDependency info = NodeDependency [(info, G)]
   deriving (Show, Eq, Ord)
   deriving (Monoid, Semigroup) via [(info, G)]

-- | one intermediate structure for constraint generation algorithm
data StageConstraint nodes edges info a = StageConstraint
   { _atInfo   :: a
   , _atRoot   :: Hole nodes info
   , _atGraph  :: CoreG nodes edges info
   , _atDep    :: NodeDependency info
   }

-- | a helper for generating application constraint
data Mode = Poly | Mono deriving (Show, Eq, Ord)

-- | companion type family
type ConstrainGraphic :: (Type -> Type) -> Type -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> Constraint
type family ConstrainGraphic f source m nodes edges info

-- | generation class
class ConstraintGen f a info | f a -> info where
  stageConstraint :: ConstrainGraphic f a m nodes edges info
                  => f (m (StageConstraint nodes edges info a))
                  -> m (StageConstraint nodes edges info a)

-- ** runner for constraint generator
genConstraint
   :: ( target ~ (Mode, ExprF stcs prms injs name |: Hole nodes Int) -- ^ annotate each expression with type node
      , ConstraintGen prms target Int, ConstrainGraphic prms target m nodes edges Int
      , ConstraintGen stcs target Int, ConstrainGraphic stcs target m nodes edges Int
      , ConstraintGen injs target Int, ConstrainGraphic injs target m nodes edges Int
      , Uno Sub :<: edges, Uno (Bind name) :<: edges, Uno Instance :<: edges, Uno Unify :<: edges
      , Uno NodeBot :<: nodes, Uno G :<: nodes
      , Ord (edges (Link edges))
      , Traversable prms, Traversable stcs, Traversable injs
      , Show name, Eq name
      , HasState "node" Int m
      , HasThrow "fail" ConstraintGenErr m
      , HasReader "local" [(name, (Uno Unify :+: Uno Instance) |: (Mode, Hole nodes Int))] m
      )
   => Expr stcs prms injs name -> m (StageConstraint nodes edges Int target)
genConstraint = cata go
  where
    go (ExRefF name) = do
      val'maybe <- asks @"local" (lookup name)
      g <- node' (Uno $ G 1)
      var <- node' (Uno $ NodeBot)
      let gr = overlays
             [ g -<< Uno (Sub 1) >>- var
             , var -<< Uno (Bind Flexible 1 $ Just name) >>- g
             ]
      case val'maybe of
        Just ((mode, n) :| Inl (Uno Unify)) ->
          return $ StageConstraint (mode, g :| ExRefF name) g
            (overlays [gr, n -<< Uno Unify >>- var, var -<< Uno Unify >>- n]) mempty
        Just ((mode, n) :| Inr (Uno (Instance i))) ->
          return $ StageConstraint (mode, g :| ExRefF name) g
            (overlays [gr, n -<< Uno (Instance i) >>- var]) mempty
        Nothing -> failConstraintMsg $ "name not in scope: " <> show name
    go (ExPrmF v) = stageConstraint v
    go (ExStcF v) = stageConstraint v
    go (ExInjF v) = stageConstraint v

-- ** instances

-- | Constraint for `Apply`
type instance ConstrainGraphic Apply (Mode, ExprF stcs prms injs name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Apply :<: stcs
    , Uno (Bind name) :<: edges, Uno Sub :<: edges, Uno Instance :<: edges, Uno Unify :<: edges
    , Uno G :<: nodes, Uno NodeBot :<: nodes, Uno NodeArr :<: nodes, Uno NodeApp :<: nodes
    , HasThrow "fail" ConstraintGenErr m
    , HasState "node" Int m
    , Ord (edges (Link edges))
    , Eq (nodes (Hole nodes Int))
    )
-- | Constraint for `Apply`
instance ConstraintGen Apply (Mode, ExprF stcs prms injs name |: Hole nodes Int) Int where
  stageConstraint (Apply ma mb ms) = do
    -- get results from subnodes
    StageConstraint (mode'a, a) r'a gr'a dep'a <- ma
    StageConstraint (mode'b, b) r'b gr'b dep'b <- mb
    ss <- sequence ms
    -- start generating constraint
    start <- genApp (mode'a, r'a, gr'a) (mode'b, r'b, gr'b)
    (mode, g, gr) <- foldM genApp start ((\(StageConstraint (mode, _) r gr _) -> (mode, r, gr)) <$> ss)
    return $ StageConstraint (mode, g :| (ExStcF . inj . Apply a b $ snd . _atInfo <$> ss)) g gr
      (dep'a <> dep'b <> mconcat (_atDep <$> ss))
    where

      -- handle instance edge in expression application
      genInstance Poly r gr to = return $ overlays [gr, r -<< Uno (Instance 1) >>- to]
      genInstance Mono r gr to = getInstance 1 r gr <&> \n -> overlays [gr, n -<< Uno Unify >>- to, to -<< Uno Unify >>- n]

      -- collect graph and generate constraint
      genApp (mode'a, r'a, gr'a) (mode'b, r'b, gr'b) = do
        g <- node' (Uno $ G 1)
        (app, arr, domain, codomain) <-
           (,,,) <$> node' (Uno $ NodeApp 3)
                 <*> node' (Uno NodeArr)
                 <*> node' (Uno NodeBot) <*> node' (Uno NodeBot)
        -- gen part of graph, including subnode graph
        gr1 <- genInstance mode'a r'a gr'a app
        gr2 <- genInstance mode'b r'b gr'b domain
        let gr = overlays
               [ app -<< Uno (Sub 1) >>- arr
               , app -<< Uno (Sub 2) >>- domain
               , app -<< Uno (Sub 3) >>- codomain
               , g -<< Uno (Sub 1) >>- codomain
               , overlays $ [r'a, r'b, app, arr, domain, codomain] <&> \n -> n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g
               , gr1, gr2
               ]
        return (mode'a, g, gr)

getInstance :: (Uno G :<: ns, Uno Sub :<: es, Ord (es (Link es)), HasThrow "fail" ConstraintGenErr m, Eq info, Eq (ns (Hole ns info)))
            => Integer -> Hole ns info -> CoreG ns es info -> m (Hole ns info)
getInstance ix n@(Hole v _) gr =
  case prj @(Uno G) v of
    Just (Uno (G i)) ->
      case lookup (Uno (Sub ix)) $ lFrom @(Uno Sub) (== n) gr of
        Just n' -> return n'
        Nothing -> if i < ix then failConstraintMsg "Internal Error, G node doesn't have sufficient instance"
                             else failConstraintMsg "Internal Error, G node doesn't have that instance"
    Nothing -> failConstraintMsg "Internal Error, Expect G node, but it is not"

type instance
  ConstrainGraphic
    (Let (Pattern plit pinj label name))
    (Mode, ExprF stcs prms injs name |: Hole ns Int)
    m nodes edges info
  = ( ns ~ nodes
    )
instance ConstraintGen (Let (Pattern plit pinj label name)) (ExprF stcs prms injs name |: Hole nodes Int) Int where
  stageConstraint = undefined

-- | expression literal
type instance ConstrainGraphic (Literal t) (ExprF stcs prms injs name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Literal t :<: prms
    , Uno G :<: nodes, Uno (NodeLit t) :<: nodes
    , Uno Sub :<: edges, Uno (Bind name) :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    )
-- | expression literal
instance ConstraintGen (Literal t) (ExprF stcs prms injs name |: Hole nodes Int) Int where
  stageConstraint (Literal t) = do
    g <- node' (Uno $ G 1)
    n <- node' (Uno $ NodeLit t)
    return $ StageConstraint (g :| ExPrmF (inj $ Literal t))
      g (overlays [g -<< Uno (Sub 1) >>- n, n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g])
      mempty

-- | expression Tuple literal
type instance ConstrainGraphic Tuple (Mode, ExprF stcs prms injs name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Tuple :<: prms
    , Uno G :<: nodes, Uno NodeTup :<: nodes
    , Uno Sub :<: edges, Uno (Bind name) :<: edges
    , Ord (edges (Link edges)), Eq (nodes (Hole nodes Int))
    , HasState "node" Int m
    , HasThrow "fail" ConstraintGenErr m
    )
-- | expression Tuple literal
instance ConstraintGen Tuple (Mode, ExprF stcs prms injs name |: Hole nodes Int) Int where
  stageConstraint (Tuple sma) = do
    let len = toInteger $ length sma
    g <- node' (Uno $ G 1)
    tup <- node' (Uno $ NodeTup len)
    sia <- zip [1..len] <$> sequence sma
    gr <- overlays <$> forM sia \(ix, StageConstraint _ g' gr' _) -> do
      n <- getInstance 1 g' gr'
      return $ overlays
        [ tup -<< Uno (Sub ix) >>- n
        , g' -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g
        , gr'
        ]
    return $ StageConstraint (Poly, g :| ExPrmF (inj . Tuple $ snd . _atInfo . snd <$> sia))
      g (overlays [gr, g -<< Uno (Sub 1) >>- tup, tup -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g])
      (mconcat $ _atDep . snd <$> sia)

-- *** Generate Pattern Constraint

data PatternData lits injs nodes label name expr = PatternData
   { patterns :: Base (Pattern lits injs label name (Mode, Base expr |: Hole nodes Int)) |: Hole nodes Int
   , bindings :: [(name, (Instance, Hole nodes Int))]
   }

node :: HasState "node" Int m => m Int
node = modify @"node" (+1) >> get @"node"
node' :: forall nodes sub m. (HasState "node" Int m, sub :<: nodes) => sub (Hole nodes Int) -> m (Hole nodes Int)
node' v = node <&> hole' v

genPatternConstraint
  :: forall lits injs label name a a' target m nodes edges stcs prms einjs
  .  ( -- for pattern
       target ~ (PatternData lits injs nodes label name a)
     , ConstraintGen lits target Int, ConstrainGraphic lits target m nodes edges Int
     , ConstraintGen injs target Int, ConstrainGraphic injs target m nodes edges Int

      -- for expression
     , a ~ (Expr stcs prms einjs name), a' ~ (Mode, Base a |: Hole nodes Int)
     , ConstraintGen prms a' Int, ConstrainGraphic prms a' m nodes edges Int
     , ConstraintGen stcs a' Int, ConstrainGraphic stcs a' m nodes edges Int
     , ConstraintGen einjs a' Int, ConstrainGraphic einjs a' m nodes edges Int

     , Uno Sub :<: edges, Uno (Bind name) :<: edges, Uno Instance :<: edges, Uno Unify :<: edges
     , Uno NodeBot :<: nodes, Uno G :<: nodes, Uno NodeArr :<: nodes, Uno NodeApp :<: nodes
     , Uno NodeTup :<: nodes, Uno NodeRec :<: nodes, Uno (NodeHas label) :<: nodes
     , Ord (edges (Link edges))
     , Eq (nodes (Hole nodes Int))
     , Show name, Eq name
     , Traversable lits, Traversable injs
     , Traversable stcs, Traversable prms, Traversable einjs
     , HasState "node" Int m
     , HasThrow "fail" ConstraintGenErr m
     , HasReader "local" [(name, (Uno Unify :+: Uno Instance) |: (Mode, Hole nodes Int))] m
     )
  => Pattern lits injs label name a -> m (StageConstraint nodes edges Int target)

genPatternConstraint = cata go
  where
    getNodeM n@(Hole v _) gr =
      case prj @(Uno G) v of
        Just _ -> case lFrom @(Uno Sub) (== n) gr of
          [(Uno (Sub 1), n')] -> return n'
          _  -> failConstraintMsg "Internal Error, G node doesn't have exact one type scheme"
        Nothing -> failConstraintMsg "Internal Error, Expect G node, but it is not"
    arrowGM = do
      app <- node' (Uno $ NodeApp 3)
      arr <- node' (Uno $ NodeArr)
      domain <- node' (Uno $ NodeBot)
      codomain <- node' (Uno $ NodeBot)
      let gr = overlays
             [ app -<< Uno (Sub 1) >>- arr
             , app -<< Uno (Sub 2) >>- domain
             , app -<< Uno (Sub 3) >>- codomain
             , Connect (link . Uno . Bind Flexible 1 $ Nothing @name)
                 (overlays $ Vertex <$> [arr, domain, codomain])
                 (Vertex app)
             ]
      return ((app, domain, codomain), gr)

    -- runner
    go PatWildF = do
      g <- node <&> hole' (Uno $ G 1)
      var <- node <&> hole' (Uno NodeBot)
      return $ StageConstraint (PatternData (g :| PatWildF) []) g
        (overlays [ g -<< Uno (Sub 1) >>- var
                  , var -<< (Uno . Bind Flexible 1 $ Nothing @name) >>- g
                  ]) mempty
    go (PatUnitF) = do
      g <- node' (Uno $ G 1)
      n <- node' (Uno $ NodeTup 0)
      let patd = PatternData (g :| PatUnitF) []
          gr = overlays
               [ n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g
               , g -<< Uno (Sub 1) >>- n
               ]
      return $ StageConstraint patd g gr mempty
    go (PatVarF name) = do
      g <- node' (Uno $ G 1)
      var <- node' (Uno NodeBot)
      let gr = overlays
               [ g -<< Uno (Sub 1) >>- var
               , var -<< (Uno . Bind Flexible 1 $ Just name) >>- g
               ]
      return $ StageConstraint (PatternData (g :| PatVarF name) [(name, (Instance 1, g))]) g gr mempty
    go (PatPrmF litm) = stageConstraint litm
    go (PatTupF sma) = do
      let len = toInteger $ length sma
      g <- node' (Uno $ G 1)
      tup <- node' (Uno $ NodeTup len)
      sia <- (`zip` [1..len]) <$> sequence sma
      gr <- overlays <$> forM sia \(StageConstraint _ n gr _, i) -> do
        n' <- getNodeM n gr
        return $ overlays [gr, n -<< (Uno . Bind Flexible 1 $ Nothing @name) >>- tup, tup -<< Uno (Sub i) >>- n']
      let patd = PatternData (g :| PatTupF (sia >>= pure . patterns . _atInfo . fst)) (sia >>= bindings . _atInfo . fst)
      return $ StageConstraint patd g
        (overlays [gr, tup -<< (Uno . Bind Flexible 1 $ Nothing @name) >>- g, g -<< Uno (Sub 1) >>- tup])
        (mconcat $ _atDep . fst <$> sia)
    go (PatRecF sma) = do
      let len = toInteger $ length sma
      g <- node' (Uno $ G 1)
      recn <- node' (Uno $ NodeRec len)
      sia <- (`zip` [1..len]) <$> mapM sequence sma
      gr <- overlays <$> forM sia \((label, StageConstraint _ n gr _), i) -> do
        n' <- getNodeM n gr
        has <- node' (Uno $ NodeHas True label)
        return $ overlays
          [ gr
          , has -<< (Uno . Bind Flexible 1 $ Nothing @name) >>- recn
          , recn -<< Uno (Sub i) >>- has
          , has -<< Uno (Sub i) >>- n'
          , n -<< (Uno . Bind Flexible 1 $ Nothing @name) >>- has
          ]
      let patd = PatternData (g :| PatRecF (sia >>= pure . fmap (patterns . _atInfo) . fst))
                             (sia >>= bindings . _atInfo . snd . fst)
      return $ StageConstraint patd g
        (overlays [gr, recn -<< (Uno . Bind Flexible 1 $ Nothing @name) >>- g, g -<< Uno (Sub 1) >>- recn])
        (mconcat $ _atDep . snd . fst <$> sia)
    go (PatSymF _ _) = error "wait for whole pass being completed"
    go (PatViewF e ma) = do
      g <- node <&> hole' (Uno $ G 1)
      ((app, domain, codomain), arr'gr) <- arrowGM
      StageConstraint (PatternData pat'a pat'bindings) pat'root pat'gr pat'dep <- ma
      StageConstraint e' e'root e'gr e'dep :: StageConstraint nodes edges Int a' <- genConstraint e
      pat' <- getNodeM pat'root pat'gr
      let patd = PatternData (g :| PatViewF e' pat'a) pat'bindings
          gr = overlays
              [ arr'gr, pat'gr, e'gr
              -- connect result node
              , g -<< Uno (Sub 1) >>- domain
              , app -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g
              -- connect right pattern node
              , pat'root -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g
              , codomain -<< Uno Unify >>- pat'
              , pat' -<< Uno Unify >>- codomain
              -- connect left expr node
              , e'root -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g
              , e'root -<< Uno (Instance 1) >>- app
              ]
      return $ StageConstraint patd g gr (e'dep <> pat'dep)
    go (PatBindF name ma) = do
      g <- node' (Uno $ G 1)
      var <- node' (Uno NodeBot)
      StageConstraint a a'n a'gr a'dep <- ma
      n' <- getNodeM a'n a'gr
      return $ StageConstraint (PatternData (g :| PatBindF name (patterns a)) ((name, (Instance 1, g)) :bindings a))
        g ( overlays
            [ a'gr
            -- bind new bottom node
            , g -<< Uno (Sub 1) >>- var
            , var -<< Uno (Bind Flexible 1 (Just name)) >>- g
            -- unify two type nodes
            , var -<< Uno Unify >>- n', n' -<< Uno Unify >>- var
            -- bind sub G node to root
            , a'n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g
            ]
          ) a'dep
    go (PatExtF injm) = stageConstraint injm

-- | handle Literal text
type instance ConstrainGraphic LiteralText (PatternData lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralText :<: lits
    , Uno (NodeLit Text) :<: nodes, Uno G :<: nodes
    , Uno (Bind name) :<: edges, Uno Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    )
instance ConstraintGen LiteralText (PatternData lits injs nodes label name expr) Int where
  stageConstraint (LiteralText (Literal t)) = do
    g :: Hole nodes Int <- node' (Uno $ G 1)
    n :: Hole nodes Int <- node' (Uno $ NodeLit t)
    let patd = PatternData (g :| PatPrmF (inj . LiteralText $ Literal t)) mempty
    return $ StageConstraint patd
      g (overlays [g -<< Uno (Sub 1) >>- n, n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g])
      mempty

-- | handle general literal
type instance ConstrainGraphic (Literal t) (PatternData lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , Literal t :<: lits
    , Uno (NodeLit t) :<: nodes, Uno G :<: nodes
    , Uno (Bind name) :<: edges, Uno Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    )
instance ConstraintGen (Literal t) (PatternData lits injs nodes label name expr) Int where
  stageConstraint (Literal t) = do
    g :: Hole nodes Int <- node' (Uno $ G 1)
    n :: Hole nodes Int <- node' (Uno $ NodeLit t)
    let patd = PatternData (g :| PatPrmF (inj $ Literal t)) mempty
    return $ StageConstraint patd
      g (overlays [g -<< Uno (Sub 1) >>- n, n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g])
      mempty

-- | handle Literal integer
type instance ConstrainGraphic LiteralInteger (PatternData lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralInteger :<: lits
    , Uno (NodeLit Integer) :<: nodes, Uno G :<: nodes
    , Uno (Bind name) :<: edges, Uno Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    )
instance ConstraintGen LiteralInteger (PatternData lits injs nodes label name expr) Int where
  stageConstraint (LiteralInteger (Literal t)) = do
    g :: Hole nodes Int <- node' (Uno $ G 1)
    n :: Hole nodes Int <- node' (Uno $ NodeLit t)
    let patd = PatternData (g :| PatPrmF (inj . LiteralInteger $ Literal t)) mempty
    return $ StageConstraint patd
      g (overlays [g -<< Uno (Sub 1) >>- n, n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g])
      mempty

-- | handle Literal number
type instance ConstrainGraphic LiteralNumber (PatternData lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralNumber :<: lits
    , Uno (NodeLit Double) :<: nodes, Uno G :<: nodes
    , Uno (Bind name) :<: edges, Uno Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    )
instance ConstraintGen LiteralNumber (PatternData lits injs nodes label name expr) Int where
  stageConstraint (LiteralNumber (Literal t)) = do
    g :: Hole nodes Int <- node' (Uno $ G 1)
    n :: Hole nodes Int <- node' (Uno $ NodeLit t)
    let patd = PatternData (g :| PatPrmF (inj . LiteralNumber $ Literal t)) mempty
    return $ StageConstraint patd
      g (overlays [g -<< Uno (Sub 1) >>- n, n -<< Uno (Bind Flexible 1 $ Nothing @name) >>- g])
      mempty


-- solveConstraint

-- infer raw = do
--   (expr, constraint) <- genConstraint raw
--   g <- solveConstraint constraint
--   getResult expr g
