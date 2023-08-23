{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $" #-}
module Language.Constraint.Graphic
  (

  -- ** all in one constraint generation class
    ConstraintGen (..)
  , ConstrainGraphic

  -- ** helpful structure to represent staged constraint
  , StageConstraint (..)
  , atInfo, atRoot, atScope, atGraph

  -- ** error handler
  , ConstraintGenErr (..)
  , failCGenMsg

  -- ** pattern constraint source
  , PatternInfo (..)
  , bindings, pattern

  -- ** environment
  --
  -- this is crucial and used to regulate definition of driver
  , HasBinding
  , BindingTable

  , HasNodeCreater

  -- ** driver code
  , genConstraint
  , genPatternConstraint
  , solve
  )
where

import Language.Core hiding (Type, Constraint)
import Language.Core.Extension

import Graph.Core
import Graph.Extension.GraphicType
import Language.Generic ((:<:), inj, prj, (:+:) (..), type (|:) (..), Base)

import Capability.State (HasState, get, modify)
import Capability.Reader (HasReader, asks)
import Capability.Error (HasThrow, throw)
import Control.Monad (forM, foldM, join)
import Control.Lens (makeLenses, (^..), _2, _1, (%~), (^.), (&))
import Data.Functor.Foldable (cata)
import Data.Functor ((<&>))

import Data.Kind (Constraint, Type)
import Data.Text (Text)
import Data.List (intersect, union, nub)
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- ** Data structures used to serve graphic constraint generation and constraint solving
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

data ConstraintGenErr
   = FailGenMsg String
   | FailUnexpect
   deriving (Show, Eq, Ord)

failCGenMsg :: HasThrow "fail" ConstraintGenErr m => String -> m a
failCGenMsg = throw @"fail" . FailGenMsg

-- | one intermediate structure for constraint generation algorithm
data StageConstraint nodes edges info a = StageConstraint
   { _atInfo   :: a
   , _atRoot   :: Hole nodes info
   , _atScope  :: ([Hole nodes info], Hole nodes info) -- ^ (nodes which need to be unified, dep node)
   , _atGraph  :: CoreG nodes edges info
   }
makeLenses ''StageConstraint

-- | companion type family for `ConstraintGen`
type ConstrainGraphic :: (Type -> Type) -> Type -> (Type -> Type) -> (Type -> Type) -> (Type -> Type) -> Type -> Constraint
type family ConstrainGraphic f source m nodes edges info

-- | generation class
class ConstraintGen f a info | f a -> info where
  stageConstraint :: ConstrainGraphic f a m nodes edges info
                  => f (m (StageConstraint nodes edges info a))
                  -> m (StageConstraint nodes edges info a)

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- ** Helper functions and Definition of Environments
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

type HasNodeCreater m = HasState "node" Int m
newNodeInfo :: HasNodeCreater m => m Int
newNodeInfo = modify @"node" (+1) >> get @"node"
node :: forall node nodes m. (HasNodeCreater m, node :<: nodes)
     => node (Hole nodes Int) -> m (Hole nodes Int)
node v = newNodeInfo <&> hole  @node v

-- | environment for local binding
type HasBinding name nodes m = HasReader "binding" (BindingTable name nodes) m
-- | formats for local binding environment
type BindingTable name nodes = [(name, (T Unify :+: T Instance) |: Hole nodes Int)]

-- | query local binding
lookupBinding
  :: (HasBinding name nodes m, Eq name)
  => name -> m (Maybe ((T Unify :+: T Instance) |: Hole nodes Int))
lookupBinding name = asks @"binding" (lookup name)

-- | get instance of a graphic type scheme
getInstance :: (G :<: ns, T Sub :<: es, Ord (es (Link es)), HasThrow "fail" ConstraintGenErr m, Ord info, Ord (ns (Hole ns info)))
            => Integer -> Hole ns info -> CoreG ns es info -> m (Hole ns info)
getInstance ix n@(Hole v _) gr =
  case prj @G v of
    Just (G i) ->
      case lookup (T (Sub ix)) $ lFrom @(T Sub) (== n) gr of
        Just n' -> return n'
        Nothing -> if i < ix then failCGenMsg "Internal Error, G node doesn't have sufficient instance"
                             else failCGenMsg "Internal Error, G node doesn't have that instance"
    Nothing -> failCGenMsg "Internal Error, Expect G node, but it is not"

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- ** Implementation of actual constraint generation logic
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- | toplevel generator for expression
genConstraint
   :: ( target ~ (ExprF f name |: Hole nodes Int) -- ^ annotate each expression with type node
      , ConstraintGen f target Int, ConstrainGraphic f target m nodes edges Int
      , T Sub :<: edges, T (Binding name) :<: edges, T Instance :<: edges, T Unify :<: edges
      , T NodeBot :<: nodes, G :<: nodes
      , Pht NDOrderLink :<: edges, NDOrder :<: nodes, Pht Sub :<: edges
      , Ord (edges (Link edges)), Traversable f
      , Show name, Eq name
      , HasThrow "fail" ConstraintGenErr m
      , HasNodeCreater m
      , HasBinding name nodes m
      )
   => Expr f name -> m (StageConstraint nodes edges Int target)
genConstraint = cata go
  where
    go (ValF name) = do
      val'maybe <- lookupBinding name
      g <- node (G 1)
      o1 <- node NDOrder -- root of order tree
      var <- node (T NodeBot)
      let gr = overlays
             [ g -<< T (Sub 1) >>- var
             , var -<< T (Binding Flexible 1 $ Just name) >>- g
             , o1 -++ Pht NDOrderLink ++- g
             ]
      case val'maybe of
        Just (n :| Inl (T Unify)) -> do
          o2 <- node NDOrder
          return $ StageConstraint (g :| ValF name) g ([n, var], o1) $ overlays
            [ gr, n -++ T Unify ++- var
            , o2 -++ Pht NDOrderLink ++- n
            , o1 -<< Pht (Sub 1) >>- o2
            ]
        Just (n :| Inr (T (Instance i))) -> do
          o2 <- node NDOrder
          return $ StageConstraint (g :| ValF name) g ([], o1) $ overlays
            [ gr, n -<< T (Instance i) >>- var
            , o2 -++ Pht NDOrderLink ++- n
            , o1 -<< Pht (Sub 1) >>- o2
            ]
        Nothing -> failCGenMsg $ "name not in scope: " <> show name
    go (ExprF v) = stageConstraint v

------------------------------------------------------------------------------------------------
-- ** instances definition for constraint generation of expression
------------------------------------------------------------------------------------------------

-- | Constraint for `Apply`
type instance ConstrainGraphic Apply (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Apply :<: f
    , T (Binding name) :<: edges, T Sub :<: edges, T Instance :<: edges, T Unify :<: edges
    , G :<: nodes, T NodeBot :<: nodes, T NodeArr :<: nodes, T NodeApp :<: nodes
    , HasThrow "fail" ConstraintGenErr m
    , HasNodeCreater m
    , Ord (edges (Link edges))
    , Eq (nodes (Hole nodes Int))
    , Ord info, Ord (ns (Hole nodes info))
    , NDOrder :<: nodes, Pht Sub :<: edges, Pht NDOrderLink :<: edges
    )
-- | Constraint for `Apply`
instance ConstraintGen Apply (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Apply ma mb ms) = do
    -- get results from subnodes
    StageConstraint a r'a dep'a gr'a <- ma
    StageConstraint b r'b dep'b gr'b <- mb
    ss <- sequence ms
    -- start generating constraint
    start <- genApp ( r'a, dep'a, gr'a) (r'b, dep'b, gr'b)
    (g, depR, gr) <- foldM genApp start ((\(StageConstraint _ r dep gr) -> (r, dep, gr)) <$> ss)
    return $ StageConstraint (g :| (ExprF . inj . Apply a b $ ss ^.. traverse . atInfo)) g depR gr
    where

      -- handle instance edge in expression application
      genInstance r gr to = return $ overlays [gr, r -<< T (Instance 1) >>- to]

      -- collect graph and generate constraint
      genApp (r'a, (dep'as, dep'a), gr'a) (r'b, (dep'bs, dep'b), gr'b) = do
        g <- node (G 1)
        depR <- node NDOrder
        (app, arr, domain, codomain) <-
           (,,,) <$> node (T $ NodeApp 3)
                 <*> node (T NodeArr)
                 <*> node (T NodeBot)
                 <*> node (T NodeBot)
        -- gen part of graph, including subnode graph
        gr1 <- genInstance r'a gr'a app
        gr2 <- genInstance r'b gr'b domain
        let gr = overlays
               [ app -<< T (Sub 1) >>- arr
               , app -<< T (Sub 2) >>- domain
               , app -<< T (Sub 3) >>- codomain
               , g -<< T (Sub 1) >>- codomain
               , overlays $ [r'a, r'b, app, arr, domain, codomain] <&> \n -> n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
               , depR -++ Pht NDOrderLink ++- g
               , depR -<< Pht (Sub 1) >>- dep'a
               , depR -<< Pht (Sub 2) >>- dep'b
               , gr1, gr2
               ]
        return (g, (dep'as <> dep'bs, depR), gr)

type instance ConstrainGraphic
    (Let (Pattern plit pinj label name))
    (ExprF f name |: Hole ns Int)
    m nodes edges info
  = ( ns ~ nodes
    )
instance ConstraintGen (Let (Pattern plit pinj label name)) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint = undefined

-- | expression literal
type instance ConstrainGraphic (Literal t) (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Literal t :<: f
    , G :<: nodes, T (NodeLit t) :<: nodes
    , T Sub :<: edges, T (Binding name) :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    , NDOrder :<: nodes, Pht Sub :<: edges, Pht NDOrderLink :<: edges
    )
-- | expression literal
instance ConstraintGen (Literal t) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Literal t) = do
    g <- node (G 1)
    depR <- node NDOrder
    n <- node (T $ NodeLit t)
    return $ StageConstraint (g :| ExprF (inj $ Literal t)) g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]

-- | expression Tuple literal
type instance ConstrainGraphic Tuple (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Tuple :<: f
    , G :<: nodes, T NodeTup :<: nodes, T NodeBot :<: nodes
    , T Sub :<: edges, T (Binding name) :<: edges
    , Ord (edges (Link edges)), Eq (nodes (Hole nodes Int))
    , HasState "node" Int m
    , HasThrow "fail" ConstraintGenErr m
    , Ord info, Ord (nodes (Hole nodes info))
    , NDOrder :<: nodes, Pht Sub :<: edges, Pht NDOrderLink :<: edges
    )
-- | expression Tuple literal
instance ConstraintGen Tuple (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Tuple sma) = do
    let len = toInteger $ length sma
    g <- node (G 1)
    tup <- node (T $ NodeTup len)
    depR <- node NDOrder
    sia <- zip [1..len] <$> sequence sma
    gr <- overlays <$> forM sia \(ix, StageConstraint _ g' (_, gdep) gr') -> do
      var <- node (T NodeBot)
      return $ overlays
        [ tup -<< T (Sub ix) >>- var
        , var -<< T (Binding Flexible 1 $ Nothing @name) >>- tup
        , g' -<< T (Binding Flexible 1 $ Nothing @name) >>- g
        , depR -<< T (Sub ix) >>- gdep
        , gr'
        ]
    let gdeps = join $ sia ^.. traverse . _2 . atScope . _1
    return $ StageConstraint (g :| ExprF (inj . Tuple $ sia ^.. traverse . _2 . atInfo)) g (gdeps, depR) $ overlays
      [ gr, g -<< T (Sub 1) >>- tup
      , tup -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]


------------------------------------------------------------------------------------------------
-- ** graphic constraint generation for simple pattern
------------------------------------------------------------------------------------------------

-- | with `pattern` as annotated syntax tree and with `bindings` as exported binding
--
--
-- a pattern is a destructor which decomposes a value and exports its inner value.
data PatternInfo lits injs nodes label name expr = PatternData
   { _pattern :: Base (Pattern lits injs label name (Base expr |: Hole nodes Int)) |: Hole nodes Int
   , _bindings :: [(name, (Instance, Hole nodes Int))]
   }

makeLenses ''PatternInfo

-- | constraint generation for a single pattern
genPatternConstraint
  :: forall lits injs label name a a' target m nodes edges f
  .  ( -- for pattern
       target ~ PatternInfo lits injs nodes label name a
     , ConstraintGen lits target Int, ConstrainGraphic lits target m nodes edges Int
     , ConstraintGen injs target Int, ConstrainGraphic injs target m nodes edges Int

      -- for expression
     , a ~ Expr f name, a' ~ (Base a |: Hole nodes Int)
     , ConstraintGen f a' Int, ConstrainGraphic f a' m nodes edges Int

     , T Sub :<: edges, T (Binding name) :<: edges, T Instance :<: edges, T Unify :<: edges
     , T NodeBot :<: nodes, G :<: nodes, T NodeArr :<: nodes, T NodeApp :<: nodes
     , T NodeTup :<: nodes, T NodeRec :<: nodes, T (NodeHas label) :<: nodes
     , Ord (edges (Link edges))
     , Show name, Eq name
     , Traversable lits, Traversable injs
     , Traversable f
     , HasState "node" Int m
     , HasThrow "fail" ConstraintGenErr m
     , HasBinding name nodes m
     , Ord (nodes (Hole nodes Int))
     , Pht NDOrderLink :<: edges, NDOrder :<: nodes, Pht Sub :<: edges
     )
  => Pattern lits injs label name a -> m (StageConstraint nodes edges Int target)

genPatternConstraint = cata go
  where
    getInstance1 = getInstance 1
    arrowGM = do
      app <- node (T $ NodeApp 3)
      arr <- node (T NodeArr)
      domain <- node (T NodeBot)
      codomain <- node (T NodeBot)
      let gr = overlays
             [ app -<< T (Sub 1) >>- arr
             , app -<< T (Sub 2) >>- domain
             , app -<< T (Sub 3) >>- codomain
             , Connect (link . T . Binding Flexible 1 $ Nothing @name)
                 (overlays $ Vertex <$> [arr, domain, codomain])
                 (Vertex app)
             ]
      return ((app, domain, codomain), gr)

    -- runner
    go PatWildF = do
      g <- node (G 1)
      depR <- node NDOrder
      var <- node (T NodeBot)
      return $ StageConstraint (PatternData (g :| PatWildF) []) g ([], depR)
        (overlays [ g -<< T (Sub 1) >>- var
                  , var -<< (T . Binding Flexible 1 $ Nothing @name) >>- g
                  , depR -++ Pht NDOrderLink ++- g
                  ])
    go PatUnitF = do
      g <- node (G 1)
      depR <- node NDOrder
      n <- node (T $ NodeTup 0)
      let patd = PatternData (g :| PatUnitF) []
          gr = overlays
               [ n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
               , g -<< T (Sub 1) >>- n
               , depR -++ Pht NDOrderLink ++- g
               ]
      return $ StageConstraint patd g ([], depR) gr
    go (PatVarF name) = do
      g <- node (G 1)
      depR <- node NDOrder
      var <- node (T NodeBot)
      let gr = overlays
               [ g -<< T (Sub 1) >>- var
               , var -<< (T . Binding Flexible 1 $ Just name) >>- g
               , depR -++ Pht NDOrderLink ++- g
               ]
      return $ StageConstraint (PatternData (g :| PatVarF name) [(name, (Instance 1, g))]) g ([], depR) gr
    go (PatPrmF litm) = stageConstraint litm
    go (PatTupF sma) = do
      let len = toInteger $ length sma
      g <- node (G 1)
      tup <- node (T $ NodeTup len)
      depR <- node NDOrder
      sia <- (`zip` [1..len]) <$> sequence sma
      (overlays -> gr, join -> gdeps) <- unzip <$> forM sia \(StageConstraint _ n (gdeps, gdep) gr, i) -> do
        var <- node (T NodeBot)
        n' <- getInstance1 n gr
        return (overlays
          [ gr
          , var -<< (T . Binding Flexible 1 $ Nothing @name) >>- tup
          , tup -<< T (Sub i) >>- var
          , var -++ T Unify ++- n'
          , n -<< T (Binding Flexible 1 $ Nothing @name) >>- tup
          , depR -<< Pht (Sub i) >>- gdep
          ], gdeps <> [var, n'])
      let patd = PatternData (g :| PatTupF (sia ^.. traverse . _1 . atInfo . pattern))
                             (join $ sia ^.. traverse . _1 . atInfo . bindings)
      return $ StageConstraint patd g (gdeps, depR)
        (overlays [ gr, tup -<< (T . Binding Flexible 1 $ Nothing @name) >>- g
                  , g -<< T (Sub 1) >>- tup
                  , depR -++ Pht NDOrderLink ++- g
                  ])
    go (PatRecF sma) = do
      let len = toInteger $ length sma
      g <- node (G 1)
      recn <- node (T $ NodeRec len)
      depR <- node NDOrder
      sia <- (`zip` [1..len]) <$> mapM sequence sma
      (overlays -> gr, join -> gdeps) <- unzip <$> forM sia \((label, StageConstraint _ n (gdeps, gdep) gr), i) -> do
        n' <- getInstance1 n gr
        has <- node (T $ NodeHas True label)
        var <- node (T NodeBot)
        return (overlays
          [ gr
          , has -<< (T . Binding Flexible 1 $ Nothing @name) >>- recn
          , recn -<< T (Sub i) >>- has
          , has -<< T (Sub i) >>- var
          , n -<< (T . Binding Flexible 1 $ Nothing @name) >>- recn
          , n' -++ T Unify ++- var
          , depR -<< Pht (Sub i) >>- gdep
          ], gdeps <> [n', var])
      let patd = PatternData (g :| PatRecF (sia ^.. traverse . _1 & traverse . _2 %~ (^. (atInfo . pattern))))
                             (join $ sia ^.. traverse . _1 . _2 . atInfo . bindings)
      return $ StageConstraint patd g (gdeps, depR) $ overlays
        [ gr
        , recn -<< (T . Binding Flexible 1 $ Nothing @name) >>- g
        , g -<< T (Sub 1) >>- recn
        , depR -++ Pht NDOrderLink ++- g
        ]
    go (PatSymF _ _) = error "wait for whole pass being completed"
    go (PatViewF e ma) = do
      g <- node (G 1)
      -- common pattern for arrow, with domain and codomain binds to root of type
      ((app, domain, codomain), arr'gr) <- arrowGM
      -- pattern constraint
      StageConstraint (PatternData pat'a pat'bindings) pat'root (pat'deps, pat'dep) pat'gr <- ma
      -- expression constraint
      StageConstraint e' e'root (e'deps, e'dep) e'gr :: StageConstraint nodes edges Int a' <- genConstraint e
      -- get instance of pattern G node, what we will use to unify with codomain
      pat' <- getInstance1 pat'root pat'gr
      -- generate NDOrder tracker node
      depR <- node NDOrder
      let patd = PatternData (g :| PatViewF e' pat'a) pat'bindings
          gr = overlays
              [ arr'gr, pat'gr, e'gr
              -- connect result node
              , g -<< T (Sub 1) >>- domain
              , app -<< T (Binding Flexible 1 $ Nothing @name) >>- g
              -- connect right pattern node
              , pat'root -<< T (Binding Flexible 1 $ Nothing @name) >>- g
              , pat' -++ T Unify ++- codomain
              -- connect left expr node
              , e'root -<< T (Binding Flexible 1 $ Nothing @name) >>- g
              , e'root -<< T (Instance 1) >>- app
              -- connect tracker node
              , depR -<< Pht (Sub 1) >>- e'dep
              , depR -<< Pht (Sub 2) >>- pat'dep
              ]
      return $ StageConstraint patd g (e'deps <> pat'deps, depR) gr
    go (PatBindF name ma) = do
      StageConstraint a g dep gr <- ma
      let patInfo = StageConstraint
                  ( PatternData
                    (g :| PatBindF name (a ^. pattern))
                    ((name, (Instance 1, g)) :a ^. bindings)
                  )
      return $ patInfo g dep gr
    go (PatExtF injm) = stageConstraint injm

------------------------------------------------------------------------------------------------
-- ** instances definition for constraint generation of simple pattern
------------------------------------------------------------------------------------------------

-- | handle Literal text
type instance ConstrainGraphic LiteralText (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralText :<: lits
    , T (NodeLit Text) :<: nodes, G :<: nodes
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    , Pht NDOrderLink :<: edges, NDOrder :<: nodes
    )
instance ConstraintGen LiteralText (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralText (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeLit t)
    depR <- node NDOrder
    let patd = PatternData (g :| PatPrmF (inj . LiteralText $ Literal t)) mempty
    return $ StageConstraint patd g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]

-- | handle general literal
type instance ConstrainGraphic (Literal t) (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , Literal t :<: lits
    , T (NodeLit t) :<: nodes, G :<: nodes
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    , Pht NDOrderLink :<: edges, NDOrder :<: nodes
    )
instance ConstraintGen (Literal t) (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (Literal t) = do
    g <- node (G 1)
    n <- node (T $ NodeLit t)
    depR <- node NDOrder
    let patd = PatternData (g :| PatPrmF (inj $ Literal t)) mempty
    return $ StageConstraint patd g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]

-- | handle Literal integer
type instance ConstrainGraphic LiteralInteger (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralInteger :<: lits
    , T (NodeLit Integer) :<: nodes, G :<: nodes
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    , Pht NDOrderLink :<: edges, NDOrder :<: nodes
    )
instance ConstraintGen LiteralInteger (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralInteger (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeLit t)
    depR <- node NDOrder
    let patd = PatternData (g :| PatPrmF (inj . LiteralInteger $ Literal t)) mempty
    return $ StageConstraint patd g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]

-- | handle Literal number
type instance ConstrainGraphic LiteralNumber (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralNumber :<: lits
    , T (NodeLit Double) :<: nodes, G :<: nodes
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord (edges (Link edges))
    , HasState "node" Int m
    , Pht NDOrderLink :<: edges, NDOrder :<: nodes
    )
instance ConstraintGen LiteralNumber (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralNumber (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeLit t)
    depR <- node NDOrder
    let patd = PatternData (g :| PatPrmF (inj $ LiteralNumber $ Literal t)) mempty
    return $ StageConstraint patd g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- ** Constraint Solver
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------
-- *** Helper functions for constraint solver
------------------------------------------------------------------------------------------------

newtype ConstraintSolvErr
  = FailSolvMsg String
   deriving (Show, Eq, Ord)

type HasSolvErr m = HasThrow ConstraintSolvErr ConstraintSolvErr m

failSolvMsg :: HasSolvErr m => String -> m a
failSolvMsg = throw @ConstraintSolvErr . FailSolvMsg

-- | get constraint interior nodes, no duplication
interiorC :: forall name ns es info. (T (Binding name) :<: es, Ord info, Ord (ns (Hole ns info))) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorC gr = nub . dfs (isLinkOf @(T (Binding name))) (transpose gr)

-- | get structure interior nodes, no duplication
interiorS :: (T Sub :<: es, Ord info, Ord (ns (Hole ns info))) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorS = fmap nub . dfs (isLinkOf @(T Sub))

-- | get interior nodes, no duplication
interiorI :: forall name ns es info. (T (Binding name) :<: es, T Sub :<: es, Ord info, Ord (ns (Hole ns info))) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorI gr root = interiorC @name gr root `intersect` interiorS gr root

-- | frontier nodes, no duplication
frontierS :: forall name ns es info. (T (Binding name) :<: es, T Sub :<: es, Ord info, Ord (ns (Hole ns info)), Ord (es (Link es))) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
frontierS gr root = nub . filter (`notElem` interiors) $ lFrom @(T Sub) (`elem` interiors) gr ^.. traverse . _2
  where interiors = interiorI @name gr root

-- | copy instance of a type scheme, return the copied graphic type
copy
  :: forall name edges nodes m
  . ( T Unify :<: edges, T Instance :<: edges
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes Int))
    , T NodeBot :<: nodes, G :<: nodes
    , HasNodeCreater m, HasThrow "fail" ConstraintGenErr m, HasSolvErr m)
  => (Hole nodes Int, Instance) -> CoreG nodes edges Int
  -- ^ (root, (oldFrontiers, newFrontiers), graphic type)
  -> m (Hole nodes Int, [(Hole nodes Int, Hole nodes Int)], CoreG nodes edges Int)
copy (scheme, Instance i) gr = do
  s <- getInstance i scheme gr
  let structureI = interiorS gr s
      interiors = allInteriors `intersect` structureI
      frontiers = allFrontiers `intersect` structureI
      -- all interiorCs bound to scheme
      interiorCBinds = lTo @(T (Binding name)) (== scheme) gr & traverse %~ \(a, b) -> (b, a)
  -- we generate copies of nodes
  freshInteriors <- forM interiors \n@(Hole tag _) -> newNodeInfo <&> (n,) . Hole tag
  freshFrontiers <- forM frontiers \n -> node (T NodeBot) <&> (n,)
  sc <- case lookup s freshInteriors of
    Just sc -> return sc
    Nothing -> failSolvMsg "Implementation Error: The root of type scheme is not included"
  let gType =
        let -- assign new node number to existed nodes to keep their structure
            -- but make them different from old ones.
            replaceNodes table n = fromMaybe n (lookup n table)
            -- remove instantiation link and unification link. this is intended to be
            --  used with `induceLink`.
            filterOutConstraint e = not $ isLinkOf @(T Unify) e || isLinkOf @(T Instance) e
            -- rebind all interiors to root of gType, excluding root node of gType.
            -- and we don't need to reset binding edge of frontier nodes since
            -- those edges are not included in the copied gType.
            resetInterior (old, new) gType =
              if null (lFrom @(T (Binding name)) (== new) gType) && new /= sc
              then case lookup old interiorCBinds of
                     Just e -> overlays [gType, new -<< e >>- sc]
                     Nothing -> gType
              else gType
        in induceLink filterOutConstraint
          $ foldr (.) id (resetInterior <$> freshInteriors)
          $ replaceNodes (freshInteriors `union`  freshFrontiers)
        <$> induce (`elem` union interiors frontiers) gr
  return (sc, freshFrontiers, gType)
  where -- a `G` node may have multiple instances, so what we get here are
        -- nodes among all of its instances
        allInteriors = interiorI @name gr scheme
        allFrontiers = frontierS @name gr scheme

-- | expand a type scheme at another `G` node and return root of new gType copied
expand :: forall name edges nodes m
  . ( T Unify :<: edges, T Instance :<: edges
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord (edges (Link edges)), Ord (nodes (Hole nodes Int))
    , T NodeBot :<: nodes, G :<: nodes
    , HasNodeCreater m, HasThrow "fail" ConstraintGenErr m, HasSolvErr m)
  => (Hole nodes Int, Instance) -> Hole nodes Int -> CoreG nodes edges Int
  -- ^ (root of expanded gType, unification nodes, modified new graphic constraint)
  -> m (Hole nodes Int, [Hole nodes Int], CoreG nodes edges Int)
expand source g gr = do
  (gRoot, gFrontiers, gType) <- copy @name source gr
  return . (gRoot, gFrontiers >>= \(a,b) -> [a,b],) $ overlays
    [ gRoot -<< T (Binding Flexible 1 $ Nothing @name) >>- g
    , overlays $ gFrontiers >>= \(old, new) ->
        [ old -++ T Unify ++- new
        , new -<< T (Binding Flexible 1 $ Nothing @name) >>- g
        ]
    , gr, gType
    ]

-- | solve a `Instance` edge
solvInst :: a
solvInst = undefined

-- | solve all `Unify` edge
solvUnify :: a
solvUnify = undefined

-- | it accepts a staged constraint and return a presolution
solve :: StageConstraint nodes edges info a -> m (StageConstraint nodes edges info a)
solve (StageConstraint info root scope gr) = undefined