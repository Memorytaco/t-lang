{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use $" #-}
{-# LANGUAGE BangPatterns #-}
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
  , HasConstraintGenErr

  -- ** pattern constraint source
  , PatternInfo (..)
  , bindings, pattern

  -- ** common environment
  --
  -- this is crucial and used to regulate definition of driver
  , HasBinding
  , BindingTable
  , BindingType

  , HasNodeCreator

  -- ** solver types
  , ConstraintSolvErr (..)
  , HasSolvErr
  , HasUnifier
  , Unifier

  -- ** driver code
  , genConstraint
  , genPatternConstraint
  , solveConstraint
  , getSolutionFromConstraint
  , getSolution

  -- ** utility tools
  , interiorC
  , interiorS
  , interiorI
  , frontierS
  , copy
  , expand
  , propagate
  , recurUnify
  , solvUnify
  , solvInst
  )
where

import Language.Core hiding (Type, Constraint)
import Language.Core.Extension

import Graph.Core
import Graph.Extension.GraphicType
import Language.Generic ((:<:), (:>+:), inj, prj, (:+:) (..), type (|:) (..), strip, fromX)
import Language.Setting ( newNodeCounter, node, HasNodeCreator )

import Capability.Reader (HasReader, asks, ask, local)
import Capability.Error (HasThrow, throw)
import Control.Monad (forM, foldM, join, (<=<))
import Control.Lens (makeLenses, (^..), _2, _1, (%~), (^.), (&), (.~), _3, _4, _5, _6)
import Data.Functor.Foldable (cata)
import Data.Functor ((<&>))

import Data.Kind (Constraint, Type)
import Data.List (intersect, union, nub)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- ** Data structures used to serve graphic constraint generation and constraint solving
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- | one intermediate structure for constraint generation algorithm
data StageConstraint nodes edges info a = StageConstraint
   { _atInfo   :: a
   , _atRoot   :: Hole nodes info
   , _atScope  :: ([Hole nodes info], Hole nodes info) -- ^ (nodes which need to be unified, dep node)
   , _atGraph  :: !(CoreG nodes edges info)
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

-- | environment for local binding
type HasBinding name nodes m = HasReader "binding" (BindingTable name nodes) m
-- | formats for local binding environment
type BindingTable name nodes = [(name, BindingType nodes)]
type BindingType nodes = (T Unify :+: T Instance) |: Hole nodes Int

-- | query local binding
lookupBinding
  :: (HasBinding name nodes m, Eq name) => name -> m (Maybe (BindingType nodes))
lookupBinding name = asks @"binding" (lookup name)

-- | append local bindings
localBinding
  :: (HasBinding name nodes m) => BindingTable name nodes -> m a -> m a
localBinding binds = local @"binding" (binds <>)

-- | error data type for constraint generation, use it with `genConstraint`
data ConstraintGenErr name
   = FailGenMsg String
   | FailGenMissingVar name
   deriving (Show, Eq, Ord)

type HasConstraintGenErr name m = HasThrow ConstraintGenErr (ConstraintGenErr name) m

failCGen :: HasConstraintGenErr name m => ConstraintGenErr name -> m a
failCGen = throw @ConstraintGenErr

failCGenMsg :: HasConstraintGenErr name m => String -> m a
failCGenMsg = failCGen . FailGenMsg

-- | get instance of a graphic type scheme
getInstance :: (G :<: ns, T Sub :<: es, HasConstraintGenErr name m, HasOrderGraph ns es info)
            => Integer -> Hole ns info -> CoreG ns es info -> m (Hole ns info)
getInstance ix n@(Hole v _) gr =
  case prj @G v of
    Just (G i) ->
      case lookup (T (Sub ix)) $ lFrom @(T Sub) (== n) gr of
        Just n' -> return n'
        Nothing -> if i < ix then failCGenMsg "Internal Error, G node doesn't have sufficient instance"
                             else failCGenMsg "Internal Error, G node doesn't have that instance"
    Nothing -> failCGenMsg "Internal Error, Expect G node, but it is not"

-- | with `pattern` as annotated syntax tree and with `bindings` as exported binding
--
--
-- a pattern is a destructor which decomposes a value and exports its inner value.
data PatternInfo lits injs nodes label name a = PatternData
   { _pattern :: PatternF lits injs label name a |: Hole nodes Int
   , _bindings :: [(name, (Instance, Hole nodes Int))]
   }

makeLenses ''PatternInfo

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- ** Implementation of actual constraint generation logic
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- | toplevel generator for expression
genConstraint
   :: ( target ~ (ExprF f name |: Hole nodes Int) -- ^ annotate each expression with type node
      , ConstraintGen f target Int, ConstrainGraphic f target m nodes edges Int
      , edges :>+:
         '[ T Sub, T (Binding name), T Unify
          , T Instance, Pht NDOrderLink, Pht Sub]
      , nodes :>+: '[T NodeBot, G, NDOrder]
      , Traversable f, HasOrderEdge edges
      , Eq name
      , HasConstraintGenErr name m
      , HasNodeCreator m
      , HasBinding name nodes m
      )
   => Expr f name -> m (StageConstraint nodes edges Int target)
genConstraint = cata go
  where
    go (ValF name) = do
      -- TODO: correctly handle constraint depencency when meeting a variable
      val'maybe <- lookupBinding name
      g <- node (G 1)
      var <- node (T NodeBot)
      depR <- node NDOrder
      let gr = overlays
             [ g -<< T (Sub 1) >>- var
             , var -<< T (Binding Flexible 1 $ Just name) >>- g
             , depR -++ Pht NDOrderLink ++- g
             ]
      case val'maybe of
        Just (n :| Inl (T Unify)) -> do
          return $ StageConstraint (g :| ValF name) g ([n, var], depR) $ overlays
            [ gr, n -++ T Unify ++- var
            ]
        Just (n :| Inr (T (Instance i))) -> do
          return $ StageConstraint (g :| ValF name) g ([], depR) $ overlays
            [ gr, n -<< T (Instance i) >>- var
            ]
        Nothing -> failCGen $ FailGenMissingVar name
    go (ExprF v) = stageConstraint v

------------------------------------------------------------------------------------------------
-- ** instances definition for constraint generation of expression
------------------------------------------------------------------------------------------------

-- | Constraint for Chain
type instance ConstrainGraphic (x :+: y) (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ConstrainGraphic x (ExprF f name |: Hole ns Int) m nodes edges info
    , ConstrainGraphic y (ExprF f name |: Hole ns Int) m nodes edges info)
instance
  ( ConstraintGen x (ExprF f name |: Hole nodes Int) Int
  , ConstraintGen y (ExprF f name |: Hole nodes Int) Int
  ) => ConstraintGen (x :+: y) (ExprF f name |: Hole nodes Int) Int where
    stageConstraint (Inl m) = stageConstraint m
    stageConstraint (Inr m) = stageConstraint m

-- | Constraint for `Apply`
type instance ConstrainGraphic Apply (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Apply :<: f
    , HasNodeCreator m
    , HasOrderGraph nodes edges info
    , edges :>+: '[Pht Sub, Pht NDOrderLink, T Instance, T Sub, T (Binding name)]
    , nodes :>+: '[T NodeApp, T NodeArr, T NodeBot, NDOrder, G]
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
  = ( ns ~ nodes)
instance ConstraintGen (Let (Pattern plit pinj label name)) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint = undefined

-- TODO: complete following constraint generation
type instance ConstrainGraphic (Equation (GPatSurface t) (Prefixes name t)) (ExprF f name |: Hole ns Int) m nodes edges info
  = ()
instance ConstraintGen (Equation (GPatSurface t) (Prefixes name t)) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic (Equation (Grp (PatSurface t)) (Prefixes name t)) (ExprF f name |: Hole ns Int) m nodes edges info
  = ( Monad m
    , HasNodeCreator m
    , HasConstraintGenErr name m
    , HasOrderGraph nodes edges info
    , HasBinding name nodes m
    , Equation (Grp (PatSurface t)) (Prefixes name t) :<: f
    , edges :>+: '[T Sub, T (Binding Name), T Instance, T Unify]
    , edges :>+: '[Pht NDOrderLink, Pht Sub]
    , nodes :>+: '[T NodeBot, G, T NodeArr, T NodeApp, T NodeTup, T NodeRec, T (NodeHas Label)]
    , nodes :>+: '[T (NodeRef name), NDOrder]
    , name ~ Name, info ~ Int, ns ~ nodes
    )
instance ConstraintGen (Equation (Grp (PatSurface t)) (Prefixes name t)) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Equation _prefix br brs) = do
    -- TODO: handle local type binding
    g <- node (G 1)
    var <- node (T NodeBot)
    depR <- node NDOrder
    br'c <- handleBranch br
    br'cs <- forM brs handleBranch
    let val = (g :|) . ExprF . inj $ Equation _prefix (br'c ^. atInfo) (br'cs ^.. traverse . atInfo)
        collect (us, gr) (i, StageConstraint _ g' (us', d') gr') = do
          r <- getInstance 1 g' gr'
          return . ([r, var] <> us <> us',) $ overlays
            [ gr, gr'
            , depR -<< Pht (Sub i) >>- d'
            , r -++ T Unify ++- var
            , g' -<< T (Binding Flexible 1 $ Nothing @name) >>- g
            ]
    -- unify every branch with bottom node and need to bind
    -- every sub G to top G
    (us, gr) <- foldM collect ([], Empty) (zip [1..] $ br'c: br'cs)
    -- since we duplicate graph many times, we need to compress
    -- the graph to get a minimal representation
    return $ StageConstraint val g (us, depR) $ compress $ overlays
      [ gr
      , depR -++ Pht NDOrderLink ++- g
      , g -<< T (Sub 1) >>- var
      , var -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      ]
    where
      -- turn a pattern binds into unifiable bindings
      asBinding gr binds = forM binds \(name, (Instance i, n)) -> do
        n' <- getInstance i n gr
        return (name, n' :| Inl (T Unify))

      genLambda (r'g, (r'us, r'd), r'gr) (StageConstraint _ pat'g (pat'us, pat'd) pat'gr) = do
        g <- node (G 1)
        depR <- node NDOrder
        (app, arr, domain, codomain) <-
           (,,,) <$> node (T $ NodeApp 3)
                 <*> node (T NodeArr)
                 <*> node (T NodeBot)
                 <*> node (T NodeBot)
        pat'n <- getInstance 1 pat'g pat'gr
        return . (g, ([domain, pat'n] <> r'us <> pat'us, depR), ) $ overlays
          [ r'gr, pat'gr
          -- build skeleton
          , overlays  -- skeleton binding edge
            [ n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
            | n <- [app, arr, domain, codomain]
            ]
          , overlays {- skeleton -} $ zip [1..] [arr, domain, codomain]
              <&> \(i, n) -> app -<< T (Sub i) >>- n
          , g -<< T (Sub 1) >>- app
          -- add binding edge
          , r'g -<< T (Binding Flexible 1 $ Nothing @name) >>- g    -- pattern
          , pat'g -<< T (Binding Flexible 1 $ Nothing @name) >>- g  -- body
          -- add constraint edge
          , r'g -<< T (Instance 1) >>- codomain -- body instance
          , domain -++ T Unify ++- pat'n  -- domain unification
          -- add tracker node
          , depR -++ Pht NDOrderLink ++- g
          , depR -<< Pht (Sub 1) >>- r'd
          , depR -<< Pht (Sub 2) >>- pat'd
          ]

      -- generate a lambda here
      handleBranch (Grp mpat mpats, mbody) = do
        -- generate first pattern
        s@(StageConstraint (PatternData _ pa'binds) _ _ pa'gr) <- genPatternConstraint mpat
        pa'bindings <- asBinding pa'gr pa'binds
        (stage'pats, lbindings) <- foldM foldT ([s], pa'bindings) $ genPatternConstraint <$> mpats
        StageConstraint e e'g e'scope e'gr <- localBinding lbindings mbody
        let pats :: [(PatSurface t) (ExprF f name |: Hole nodes Int)]
            pats = reverse $ fromX . strip <$> (stage'pats ^.. traverse . atInfo . pattern)
            branch = (Grp (head pats) (tail pats), e)
        (g, scopes, gr) <- foldM genLambda (e'g, e'scope, e'gr) stage'pats
        return $ StageConstraint branch g scopes gr
          where
          -- stage constraint is built in reverse order
          foldT (ss, lbindings) mb = do
            s@(StageConstraint (PatternData _ pb'binds) _ _ pb'gr) <-
              localBinding lbindings mb
            pb'bindings <- asBinding pb'gr pb'binds
            return (s:ss, pb'bindings <> lbindings)

type instance ConstrainGraphic (Value TypSurface) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen (Value TypSurface) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic (Selector Label) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen (Selector Label) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic (LetGrp (PatSurface t)) (ExprF f name |: Hole ns Int) m nodes edges info
  = ( Monad m
    , edges :>+: '[T Sub, T (Binding Name), T Instance, T Unify]
    , edges :>+: '[Pht NDOrderLink, Pht Sub]
    , nodes :>+: '[T NodeBot, G, T NodeArr, T NodeApp, T NodeTup, T NodeRec, T (NodeHas Label)]
    , nodes :>+: '[T (NodeRef name), NDOrder]
    , HasNodeCreator m
    , HasConstraintGenErr name m
    , HasOrderGraph nodes edges Int
    , HasBinding name nodes m
    , LetGrp (PatSurface t) :<: f
    , name ~ Name, ns ~ nodes
    )
instance ConstraintGen (LetGrp (PatSurface t)) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LetGrp ms m) = do
    envs <- forM ms \(mpat, me) -> do
      StageConstraint (PatternData pat binds) p'g (p'deps, p'dep) p'gr <- genPatternConstraint mpat
      StageConstraint e e'g (e'deps, e'dep) e'gr <- me
      let sPat = (fromX $ strip pat) :: ((PatSurface t) (ExprF f name |: Hole nodes Int))
      p'root <- getInstance 1 p'g p'gr
      e'root <- getInstance 1 e'g e'gr
      return . ((sPat, e), binds, [p'root, e'root] <> e'deps <> p'deps, e'dep, e'g, )
        $ overlays
        [ p'gr, e'gr
        , e'root -++ T Unify ++- p'root
        , p'g -<< T (Binding Flexible 1 $ Nothing @name) >>- e'g
        , e'dep -<< Pht (Sub 1) >>- p'dep
        ]
    -- TODO: add duplication check for bindings
    StageConstraint e (e'g :: Hole nodes Int) (e'deps, e'dep) e'gr <-
      localBinding
        [ (name, nd :| Inr (T inst))
        | binds <- envs ^.. traverse . _2
        , (name, (inst, nd)) <- binds ]
        m
    let unification = e'deps <> (envs ^.. traverse . _3 . traverse)
        val = ExprF . inj $ LetGrp (envs ^.. traverse . _1) e
    return $ StageConstraint (e'g :| val) e'g (unification, e'dep) $ overlays
      [ e'gr
      -- build constraint depencency relation
      , overlays
        [ e'dep -<< Pht (Sub i) >>- n
        | (i, n) <- zip [1..] $ envs ^.. traverse . _4
        ]
      -- build constraint binding edges
      , overlays
        [ n -<< T (Binding Flexible 1 $ Nothing @name) >>- e'g
        | n <- envs ^.. traverse . _5
        ]
      -- merge graphs
      , overlays $ envs ^.. traverse . _6
      ]

type instance ConstrainGraphic (Let (Binder (name @: Maybe t))) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen (Let (Binder (name @: Maybe t))) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic (Letrec (Binder (name @: Maybe t))) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen (Letrec (Binder (name @: Maybe t))) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic ((@:) t) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen ((@:) t) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic LiteralText (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , nodes :>+: '[NDOrder, G, T (NodeRef name)]
    , edges :>+: '[T Sub, T (Binding name), Pht NDOrderLink]
    , HasNodeCreator m
    , IsString name
    , LiteralText :<: f
    , HasOrderEdge edges
    )
instance ConstraintGen LiteralText (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LiteralText (Literal t)) = do
    g <- node (G 1)
    depR <- node NDOrder
    n <- node (T $ NodeRef False (fromString @name "text"))
    return $ StageConstraint (g :| ExprF (inj $ LiteralText $ Literal t)) g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]
type instance ConstrainGraphic LiteralInteger (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , nodes :>+: '[NDOrder, G, T (NodeRef name)]
    , edges :>+: '[T Sub, T (Binding name), Pht NDOrderLink]
    , HasNodeCreator m
    , IsString name
    , LiteralInteger :<: f
    , HasOrderEdge edges
    )
instance ConstraintGen LiteralInteger (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LiteralInteger (Literal t)) = do
    g <- node (G 1)
    depR <- node NDOrder
    n <- node (T $ NodeRef False (fromString @name "int"))
    return $ StageConstraint (g :| ExprF (inj $ LiteralInteger $ Literal t)) g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]
type instance ConstrainGraphic LiteralNumber (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , nodes :>+: '[NDOrder, G, T (NodeRef name)]
    , edges :>+: '[T Sub, T (Binding name), Pht NDOrderLink]
    , HasNodeCreator m
    , IsString name
    , LiteralNumber :<: f
    , HasOrderEdge edges
    )
instance ConstraintGen LiteralNumber (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LiteralNumber (Literal t)) = do
    g <- node (G 1)
    depR <- node NDOrder
    n <- node (T $ NodeRef False (fromString @name "double"))
    return $ StageConstraint (g :| ExprF (inj $ LiteralNumber $ Literal t)) g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]

type instance ConstrainGraphic (Record Label) (ExprF f name |: Hole ns Int) m nodes edges info
  = ()
instance ConstraintGen (Record Label) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic (Constructor Label) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen (Constructor Label) (ExprF f name |: Hole nodes Int) Int where

-- | expression literal
type instance ConstrainGraphic (Literal t) (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Literal t :<: f
    , HasOrderEdge edges
    , HasNodeCreator m
    , edges :>+: '[Pht Sub, Pht NDOrderLink, T (Binding name), T Sub]
    , nodes :>+: '[T (NodeLit t), NDOrder, G]
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
    , HasNodeCreator m
    , HasOrderGraph nodes edges info
    , edges :>+: '[Pht Sub, Pht NDOrderLink, T (Binding name), T Sub, T Instance]
    , nodes :>+: '[NDOrder, G, T NodeBot, T NodeTup]
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
        , var -<< T (Binding Flexible 1 $ Nothing @name) >>- g
        , g' -<< T (Binding Flexible 1 $ Nothing @name) >>- g
        , depR -<< Pht (Sub ix) >>- gdep
        , g' -<< T (Instance 1) >>- var
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

-- | constraint generation for a single pattern
genPatternConstraint
  :: forall nodes edges lits injs label name expr target m
   . ( -- for pattern
       target ~ PatternInfo lits injs nodes label name expr
     , ConstraintGen lits target Int, ConstrainGraphic lits target m nodes edges Int
     , ConstraintGen injs target Int, ConstrainGraphic injs target m nodes edges Int

     , edges :>+: '[T Sub, T (Binding name), T Instance, T Unify]
     , nodes :>+: '[T NodeBot, G, T NodeArr, T NodeApp, T NodeTup, T NodeRec, T (NodeHas label)]
     , Traversable lits, Traversable injs, HasNodeCreator m
     , HasConstraintGenErr name m
     , HasOrderGraph nodes edges Int
     , Pht NDOrderLink :<: edges, NDOrder :<: nodes, Pht Sub :<: edges
     )
  => Pattern lits injs label name (m (StageConstraint nodes edges Int expr))
  -> m (StageConstraint nodes edges Int target)

genPatternConstraint = cata go
  where
    getInstance1 = getInstance 1
    functionPattern g = do
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
                 (Vertex g)
             , app -<< T (Binding Flexible 1 $ Nothing @name) >>- g
             ]
      return ((app, domain, codomain), gr)

    -- runner
    go PatWildF = do
      g <- node (G 1)
      var <- node (T NodeBot)
      depR <- node NDOrder
      return $ StageConstraint (PatternData (g :| PatWildF) []) g ([], depR)
        $ overlays
        [ g -<< T (Sub 1) >>- var
        , var -<< T (Binding Flexible 1 $ Nothing @name) >>- g
        , depR -++ Pht NDOrderLink ++- g
        ]

    go PatUnitF = do
      g <- node (G 1)
      n <- node (T $ NodeTup 0)
      depR <- node NDOrder
      return $ StageConstraint (PatternData (g :| PatUnitF) []) g ([], depR)
        $ overlays
        [ n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
        , g -<< T (Sub 1) >>- n
        , depR -++ Pht NDOrderLink ++- g
        ]

    go (PatVarF name) = do
      g <- node (G 1)
      var <- node (T NodeBot)
      depR <- node NDOrder
      return $ StageConstraint (PatternData (g :| PatVarF name) [(name, (Instance 1, g))]) g ([], depR)
        $ overlays
        [ g -<< T (Sub 1) >>- var
        , var -<< T (Binding Flexible 1 $ Just name) >>- g
        , depR -++ Pht NDOrderLink ++- g
        ]

    go (PatPrmF litm) = stageConstraint litm

    go (PatTupF sma) = do
      let len = toInteger $ length sma
      g <- node (G 1)
      tup <- node (T $ NodeTup len)
      depR <- node NDOrder
      sia <- (`zip` [1..len]) <$> sequence sma
      (join -> gdeps, overlays -> gr) <-
        unzip <$> forM sia \(StageConstraint _ n'g (n'us, n'd) n'gr, i) -> do
        var <- node (T NodeBot)
        n'root <- getInstance1 n'g n'gr
        return . (var: n'root: n'us, ) $ overlays
          [ n'gr
          -- add binding edge and structure edge
          , var -<< T (Binding Flexible 1 $ Nothing @name) >>- g
          , tup -<< T (Sub i) >>- var
          -- add unify constraint
          , var -++ T Unify ++- n'root
          -- bind sub constraint
          , n'g -<< T (Binding Flexible 1 $ Nothing @name) >>- g
          -- add constraint dependency order
          , depR -<< Pht (Sub i) >>- n'd
          ]
      let patd = PatternData (g :| PatTupF (sia ^.. traverse . _1 . atInfo . pattern))
                             (sia ^.. traverse . _1 . atInfo . bindings . traverse)
      return $ StageConstraint patd g (gdeps, depR) $ overlays
        [ gr, tup -<< T (Binding Flexible 1 $ Nothing @name) >>- g
        , g -<< T (Sub 1) >>- tup
        , depR -++ Pht NDOrderLink ++- g
        ]

    -- | TODO: recheck constraint generation rule
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
    go (PatViewF me ma) = do
      g <- node (G 1)
      -- generate NDOrder tracker node
      depR <- node NDOrder
      -- common pattern for arrow, with domain and codomain binds to root of type
      ((app, domain, codomain), arr'gr) <- functionPattern g
      -- pattern constraint
      StageConstraint (PatternData pat binds) pat'g (pat'deps, pat'dep) pat'gr <- ma
      -- expression constraint
      StageConstraint e e'g (e'deps, e'dep) e'gr <- me
      -- get instance of pattern G node, what we will use to unify with codomain
      pat'root <- getInstance1 pat'g pat'gr
      let patd = PatternData (g :| PatViewF e pat) binds
          gr = overlays
              [ arr'gr, pat'gr, e'gr
              -- connect result node
              , g -<< T (Sub 1) >>- domain
              -- connect right pattern node
              , pat'g -<< T (Binding Flexible 1 $ Nothing @name) >>- g
              , pat'root -++ T Unify ++- codomain
              -- connect left expr node
              , e'g -<< T (Binding Flexible 1 $ Nothing @name) >>- g
              , e'g -<< T (Instance 1) >>- app
              -- connect tracker node
              , depR -<< Pht (Sub 1) >>- e'dep
              , depR -<< Pht (Sub 2) >>- pat'dep
              , depR -++ Pht NDOrderLink ++- g
              ]
      return $ StageConstraint patd g ([pat'root, codomain] <> e'deps <> pat'deps, depR) gr
    go (PatBindF name ma) = do
      StageConstraint patd g dep gr <- ma
      let patInfo = StageConstraint
                  ( PatternData
                    (g :| PatBindF name (patd ^. pattern))
                    ((name, (Instance 1, g)) :patd ^. bindings)
                  )
      return $ patInfo g dep gr
    go (PatExtF injm) = stageConstraint injm

------------------------------------------------------------------------------------------------
-- ** instances definition for constraint generation of simple pattern
------------------------------------------------------------------------------------------------

type instance ConstrainGraphic (x :+: y) (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ConstrainGraphic x (PatternInfo lits injs ns label name expr) m nodes edges info
    , ConstrainGraphic y (PatternInfo lits injs ns label name expr) m nodes edges info)
instance
  ( ConstraintGen x (PatternInfo lits injs ns label name expr) Int
  , ConstraintGen y (PatternInfo lits injs ns label name expr) Int
  ) => ConstraintGen (x :+: y) (PatternInfo lits injs ns label name expr) Int where
    stageConstraint (Inl m) = stageConstraint m
    stageConstraint (Inr m) = stageConstraint m

-- | handle Literal text
type instance ConstrainGraphic LiteralText (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralText :<: lits
    , HasOrderEdge edges
    , HasNodeCreator m
    , edges :>+: '[T (Binding name), T Sub, Pht NDOrderLink]
    , nodes :>+: '[T (NodeRef name), G, NDOrder]
    , IsString name
    )
instance ConstraintGen LiteralText (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralText (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef False $ fromString @name "text")
    depR <- node NDOrder
    let patd = PatternData (g :| PatPrmF (inj . LiteralText $ Literal t)) mempty
    return $ StageConstraint patd g ([], depR) $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible 1 $ Nothing @name) >>- g
      , depR -++ Pht NDOrderLink ++- g
      ]

-- | handle type cast in pattern matching
type instance ConstrainGraphic ((@:) t) (PatternInfo lits injs ns label name expr) m nodes edges info
  = ()
instance ConstraintGen ((@:) t) (PatternInfo lits injs nodes label name expr) Int where

type instance ConstrainGraphic PatGroup (PatternInfo lits injs ns label name expr) m nodes edges info
  = ()
instance ConstraintGen PatGroup (PatternInfo lits injs nodes label name expr) Int where


-- | handle general literal
type instance ConstrainGraphic (Literal t) (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , Literal t :<: lits
    , HasOrderEdge edges
    , HasNodeCreator m
    , edges :>+: '[T (Binding name), T Sub, Pht NDOrderLink]
    , nodes :>+: '[T (NodeLit t), G, NDOrder]
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
    , HasOrderEdge edges
    , HasNodeCreator m
    , Pht NDOrderLink :<: edges, NDOrder :<: nodes
    , edges :>+: '[T (Binding name), T Sub, Pht NDOrderLink]
    , nodes :>+: '[T (NodeRef name), G, NDOrder]
    , IsString name
    )
instance ConstraintGen LiteralInteger (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralInteger (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef False $ fromString @name "int")
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
    , T (NodeRef name) :<: nodes, G :<: nodes
    , T (Binding name) :<: edges, T Sub :<: edges
    , Ord (edges (Link edges))
    , HasNodeCreator m
    , IsString name
    , Pht NDOrderLink :<: edges, NDOrder :<: nodes
    )
instance ConstraintGen LiteralNumber (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralNumber (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef False $ fromString @name "string")
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

-- TODO: add unit test for all functions below

data ConstraintSolvErr node gr err
  = FailSolvMsg String
  | FailSolvUnify err
  | FailSolvProperty gr (ConstraintSolvPropErr node)
  deriving (Show, Eq, Ord)

data ConstraintSolvPropErr node
  = CSPropMultipleNodes node [node] String
  | CSPropMultipleBinder node [node] String
  deriving (Show, Eq, Ord)

type HasSolvErr node gr err m = HasThrow ConstraintSolvErr (ConstraintSolvErr node gr err) m

failSolvMsg :: HasSolvErr nodeg gr err m => String -> m a
failSolvMsg = throw @ConstraintSolvErr . FailSolvMsg

failSolvUnify :: HasSolvErr node gr err m => err -> m a
failSolvUnify = throw @ConstraintSolvErr . FailSolvUnify

failSolvProp :: HasSolvErr node gr err m => gr -> ConstraintSolvPropErr node -> m a
failSolvProp gr = throw @ConstraintSolvErr . FailSolvProperty gr

-- | get constraint interior nodes, no duplication
interiorC :: forall name ns es info. (T (Binding name) :<: es, HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorC !gr = nub . dfs (isLinkOf @(T (Binding name))) (transpose gr)

-- | get structure interior nodes, no duplication
interiorS :: (T Sub :<: es, HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorS = fmap nub . dfs (isLinkOf @(T Sub))

-- | get interior nodes, no duplication
interiorI :: forall name ns es info. (T (Binding name) :<: es, T Sub :<: es, HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorI !gr root = interiorC @name gr root `intersect` interiorS gr root

-- | frontier nodes, no duplication
frontierS :: forall name ns es info. (es :>+: '[T (Binding name), T Sub], HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
frontierS gr root = nub . filter (`notElem` interiors) $ lFrom @(T Sub) (`elem` interiors) gr <&> snd
  where interiors = interiorI @name gr root

-- | copy instance of a type scheme, return the copied graphic type
copy
  :: forall name edges nodes err m
  . ( edges :>+: '[T Unify, T Instance, T (Binding name), T Instance, T Sub]
    , nodes :>+: '[T NodeBot, G]
    , HasOrderGraph nodes edges Int
    , HasNodeCreator m, HasConstraintGenErr name m, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m)
  => (Hole nodes Int, Instance) -> CoreG nodes edges Int
  -- ^ (root, (oldFrontiers, newFrontiers), graphic type)
  -> m (Hole nodes Int, [(Hole nodes Int, Hole nodes Int)], CoreG nodes edges Int)
copy (scheme, Instance i) !gr = do
  s <- getInstance i scheme gr
  let structureI = interiorS gr s
      !interiors = allInteriors `intersect` structureI
      !frontiers = allFrontiers `intersect` structureI
      -- all interiorCs bound to scheme
      interiorCBinds = lTo @(T (Binding name)) (== scheme) gr & traverse %~ \(a, b) -> (b, a)
  -- we generate copies of nodes
  freshInteriors <- forM interiors \n@(Hole tag _) -> newNodeCounter <&> (n,) . Hole tag
  freshFrontiers <- forM frontiers \n -> node (T NodeBot) <&> (n,)
  sc <- case lookup s freshInteriors of
    Just sc -> return sc
    Nothing -> failSolvMsg "Implementation Error: The root of type scheme is not included"
  let graphicType =
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
  return (sc, freshFrontiers, graphicType)
  where -- a `G` node may have multiple instances, so what we get here are
        -- nodes among all of its instances
        !allInteriors = interiorI @name gr scheme
        !allFrontiers = frontierS @name gr scheme

-- | expand a type scheme at another node and return root of new gType copied
expand :: forall name edges nodes err m
  . ( edges :>+: '[T Unify, T Instance, T (Binding name), T Sub]
    , T NodeBot :<: nodes, G :<: nodes
    , HasOrderGraph nodes edges Int
    , HasNodeCreator m, HasConstraintGenErr name m, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m)
  => (Hole nodes Int, Instance) -> Hole nodes Int -> CoreG nodes edges Int
  -- ^ (root of expanded gType, unification nodes, modified new graphic constraint)
  -> m (Hole nodes Int, [Hole nodes Int], CoreG nodes edges Int)
expand source target !gr = do
  (gRoot, gFrontiers, gType) <- copy @name source gr
  return . (gRoot, gFrontiers >>= \(a,b) -> [a,b],) . compress $ overlays
    [ gRoot -<< T (Binding Flexible 1 $ Nothing @name) >>- target
    , overlays $ gFrontiers >>= \(old, new) ->
        [ old -++ T Unify ++- new
        , new -<< T (Binding Flexible 1 $ Nothing @name) >>- target
        ]
    , gr, gType
    ]

-- | propagate a graphic constraint, return results constraint and new generated unifications
propagate :: forall name edges nodes err m
  . ( edges :>+: '[T Unify, T Instance, T (Binding name), T Sub]
    , T NodeBot :<: nodes, G :<: nodes
    , HasOrderGraph nodes edges Int
    , HasNodeCreator m, HasConstraintGenErr name m, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m)
  => Hole nodes Int -> CoreG nodes edges Int
  -> m ([Hole nodes Int], CoreG nodes edges Int)
propagate scheme (compress -> gr) = do
  instances <- forM constraints \(source, t) -> do
    binder <- case lFrom @(T (Binding name)) (== t) gr of
      [(_, binder)] -> return binder
      [] -> failSolvProp gr $ CSPropMultipleBinder t [] "A node has no binders during propagation"
      binders -> failSolvProp gr $ CSPropMultipleBinder t (snd <$> binders) "A node has multiple binders during propagation"
    return (source, t, binder)
  let folder (ounifications, ogr) (source, t, binder) = do
        (root, unifications, gType) <- expand @name source binder ogr
        return . (root:t:unifications <> ounifications,) $ overlays
          [ gType, ogr, root -++ T Unify ++- t ]
  foldM folder ([], gr) instances
  where
    constraints = lFrom (== scheme) gr <&> \(T (Instance i), t) ->
      ((scheme, Instance i), t)

type HasUnifier err nodes edges info m =
  HasReader "Unifier" (Unifier err nodes edges info m) m
type Unifier err nodes edges info m =
  CoreG nodes edges info
    -> Hole nodes info -> Hole nodes info
    -> m (Either err (Hole nodes info, CoreG nodes edges info))

-- | if we have a unification link represented in graph like
--
-- @@@
-- a <--> b <--> c
-- @@@
--
-- we can feed `recurUnify` with any of a, b or c and we get a single final node "x"
-- with unification edge points to itself and "x" is among a, b or c.
recurUnify
  ::  ( HasUnifier err nodes edges info m
      , HasSolvErr (Hole nodes Int) (CoreG nodes edges info) err m
      , HasOrderGraph nodes edges info, T Unify :<: edges)
  => CoreG nodes edges info -> Hole nodes info -> m (CoreG nodes edges info)
recurUnify graph start = do
  unify <- ask @"Unifier"
  case nub $ snd <$> lFrom @(T Unify) (== start) graph of
    [] -> return graph
    ls@(_:_) ->
      case filter (/= start) ls of
        [] -> return (replaceLink justUnifyLink start start graph)
        x:_ -> unify graph start x >>= \case
          Left err -> failSolvUnify err
          Right (next, gr) -> recurUnify gr next
  where
    justUnifyLink e = if isLinkOf @(T Unify) e then Nothing else Just e

-- | solve all `Unify` edges in a stage constraint
solvUnify
  ::  ( HasUnifier err nodes edges info m, edges :>+: '[T Unify]
      , HasOrderGraph nodes edges info, HasSolvErr (Hole nodes Int) (CoreG nodes edges info) err m)
  => StageConstraint nodes edges info a -> m (StageConstraint nodes edges info a)
solvUnify (StageConstraint info root (nub -> unifications, dep) graph) = do
  gr <- foldM recurUnify graph unifications
  -- gr' <- verifyUnifier gr unifications
  return (StageConstraint info root ([], dep) gr)

-- | solve a `Instance` edge
solvInst
  :: forall name nodes edges m err a
  . ( HasUnifier err nodes edges Int m, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m, HasConstraintGenErr name m
    , HasNodeCreator m
    , nodes :>+: '[NDOrder, G, T NodeBot]
    , edges :>+: '[Pht Sub, Pht NDOrderLink, T (Binding name), T Unify, T Instance, T Sub]
    , HasOrderGraph nodes edges Int )
  => StageConstraint nodes edges Int a
  -> m (StageConstraint nodes edges Int a)
solvInst sc@(StageConstraint _ _ (unifications, dep) (compress -> graph)) = do
  if null unifications
  then return ()
  else failSolvMsg "Unexpected unification constraint when trying to solve instance edges"
  let orders = dfs (isLinkOf @(Pht Sub)) graph dep
  -- TOOD: verify it is acyclic graphic constraint
  go sc (reverse orders)
  where
    go c [] = return c
    go c@(StageConstraint _ _ _ gr) (n:ns) = do
      inst <- case lFrom @(Pht NDOrderLink) (== n) gr of
        [] -> failSolvMsg "Internal error: misplaced tracker order node, no G node available"
        [(_, n')] ->
          if isHoleOf @G n'
          then return n'
          else failSolvMsg "Internal error: wrong node linked with tracker order node"
        (fmap snd -> ns') -> failSolvProp gr $ CSPropMultipleNodes n ns' "Internal error: multiple nodes linked with tracker order node"
      (unifications', gr') <- propagate @name inst gr
      c' <- solvUnify (c & atScope . _1 .~ unifications' & atGraph .~ gr')
      go c' ns

-- | it accepts a staged constraint and return a presolution
solveConstraint
  :: forall name nodes edges m err a
  . ( HasUnifier err nodes edges Int m
    , HasNodeCreator m, HasConstraintGenErr name m
    , edges :>+: '[Pht Sub, Pht NDOrderLink, T (Binding name), T Unify, T Instance, T Sub]
    , nodes :>+: '[NDOrder, G, T NodeBot]
    , HasOrderGraph nodes edges Int, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m )
  => StageConstraint nodes edges Int a -> m (StageConstraint nodes edges Int a)
solveConstraint = solvInst @name <=< solvUnify
{-# INLINE solveConstraint #-}

-- | get a graphic type from a `G` node of a presolution.
getSolution
  :: ( HasNodeCreator m
     , HasConstraintGenErr name m, HasUnifier err nodes edges Int m
     , HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m
     , nodes :>+: '[NodePht, T NodeBot, G]
     , edges :>+: '[T (Binding name), T Unify, T Instance, T Sub]
     , HasOrderGraph nodes edges Int
     )
  => (Hole nodes Int, Instance) -> CoreG nodes edges Int
  -> m (Hole nodes Int, CoreG nodes edges Int)
getSolution source !graph = do
  root <- node NodePht
  (gRoot, unifications, gr) <- expand source root graph
  gr' <- foldM recurUnify (overlays [gr, root -<< T (Sub 1) >>- gRoot]) unifications
  return (root, gr')

-- | get a solution from a constraint presolution
getSolutionFromConstraint
  :: ( HasNodeCreator m
     , HasConstraintGenErr name m, HasUnifier err nodes edges Int m
     , HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m
     , nodes :>+: '[NodePht, T NodeBot, G]
     , edges :>+: '[T (Binding name), T Unify, T Instance, T Sub]
     , HasOrderGraph nodes edges Int
     )
  => StageConstraint nodes edges Int a
  -> m (Hole nodes Int, CoreG nodes edges Int)
getSolutionFromConstraint (StageConstraint _ g _ graph)
  = getSolution (g, Instance 1) graph
