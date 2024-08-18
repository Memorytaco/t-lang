{-# LANGUAGE AllowAmbiguousTypes #-}
module Language.Constraint.Graphic
  (

  -- ** all in one constraint generation class
    ConstraintGen (..)
  , ConstrainGraphic

  -- ** helpful structure to represent staged constraint
  , StageConstraint (..)
  , stage, root, storage

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
  , expand
  , propagate
  , solvUnify
  , solvInst
  )
where

import Language.Core hiding (Type, Constraint)
import Language.Core.Extension

import Graph.Core hiding (bfs)
import Graph.Extension.GraphicType
import Language.Generic ((:<:), (:>+:), inj, prj, (:+:) (..), type (|:) (..), strip, fromX)
import Language.Setting ( newNodeCounter, node, HasNodeCreator )
import Graph.Data (order, prune, GraphT (..))
import Graph.Algorithm (bfs, topSort)

import Capability.Reader (HasReader, asks, ask, local)
import Capability.Error (HasThrow, throw)
import Control.Monad (forM, foldM, join)
import Control.Lens (makeLenses, (^..), _2, _1, _3, _4, (%~), (.~), (^.), (&))
import Data.Functor.Foldable (cata)
import Data.Functor ((<&>))

import Control.Exception (assert)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT (..), gets, modify)
import Data.Bifunctor (bimap)

import Data.Kind (Constraint, Type)
import qualified Data.Set as Set
import Data.String (IsString (fromString))

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
-- ** Data structures used to serve graphic constraint generation and constraint solving
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- | one intermediate structure for constraint generation algorithm
data StageConstraint nodes edges info a = StageConstraint
   { _stage    :: a                         -- ^ whatever the stage constraint returned
   , _root     :: Hole nodes info           -- ^ root node, it is usually G node, but depends on actual constraint
   , _storage  :: CoreG nodes edges info    -- ^ graph
   } deriving (Functor, Foldable, Traversable)
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
getInstance inst r@(Hole tag _) gr =
  case prj @G tag of
    Just (G i) ->
      case lookup (T (Sub inst)) $ lFrom @(T Sub) r gr of
        Just n -> return n
        Nothing -> if i < inst then failCGenMsg "Internal Error, G node doesn't have sufficient instance"
                             else failCGenMsg "Internal Error, G node doesn't have that instance"
    Nothing -> failCGenMsg "Internal Error, Expect G node, but it is not"

-- | G generation usually has at least one instance and this is also the default case.
--
-- This is equivalent to saying give me the default one connected to g node.
getInstance1 :: (G :<: ns, T Sub :<: es, HasConstraintGenErr name m, HasOrderGraph ns es info)
            => Hole ns info -> CoreG ns es info -> m (Hole ns info)
getInstance1 = getInstance 1

-- | with `pattern` as annotated syntax tree and with `bindings` as exported binding
--
--
-- a pattern is a destructor which decomposes a value and exports its inner value.
--
-- It also forces evaluation.
data PatternInfo prms injs nodes label name a = PatternData
   { _pattern :: PatternF prms injs label name a |: Hole nodes Int
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
      , edges :>+: '[ T Sub, T (Binding name), T Unify , T Instance, Pht Sub]
      , nodes :>+: '[T NodeBot, G]
      , Traversable f
      , Eq name
      , HasConstraintGenErr name m
      , HasNodeCreator m
      , HasBinding name nodes m
      )
   => Expr f name -> m (StageConstraint nodes edges Int target)
genConstraint = cata go
  where
    go (ValF name) = do
      val'maybe <- lookupBinding name
      g <- node (G 1)
      var <- node (T NodeBot)
      let gr = overlays
             [ g -<< T (Sub 1) >>- var
             , var -<< T (Binding Flexible $ Just name) >>- g
             ]
      case val'maybe of
        Just (n :| Inl (T Unify)) -> do
          return $ StageConstraint (g :| ValF name) g . (gr <>) $
            n -++ T Unify ++- var
        Just (n :| Inr (T (Instance i))) -> do
          return $ StageConstraint (g :| ValF name) g . (gr <>) $
            n -<< T (Instance i) >>- var
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
    , edges :>+: '[Pht Sub, T Instance, T Sub, T (Binding name)]
    , nodes :>+: '[T NodeArr, T NodeBot, G]
    )
-- | Constraint for `Apply`
instance ConstraintGen Apply (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Apply ma mb ms) = do
    -- get results from subnodes
    StageConstraint a r'a gr'a <- ma
    StageConstraint b r'b gr'b <- mb
    ss <- sequence ms
    -- start generating constraint
    start <- genApp (r'a, gr'a) (r'b, gr'b)
    (g, gr) <- foldM genApp start [(r, gr) | StageConstraint _ r gr <- ss]
    return $ StageConstraint (g :| (ExprF . inj . Apply a b $ ss ^.. traverse . stage)) g gr
    where

      -- handle instance edge in expression application
      genInstance r gr to = gr <> (r -<< T (Instance 1) >>- to)

      -- collect graph and generate constraint
      genApp (g'a, gr'a) (g'b, gr'b) = do
        g <- node (G 1)
        arr <- node (T NodeArr)
        domain <- node (T NodeBot)
        codomain <- node (T NodeBot)
        -- gen part of graph, including subnode graph
        return . (g, ) $ overlays
          [ arr -<< T (Sub 1) >>- domain
          , arr -<< T (Sub 2) >>- codomain
          , g -<< T (Sub 1) >>- codomain
          , Connect (link $ T (Binding Flexible $ Nothing @name))
              (fromVertices [g'a, g'b, arr, domain, codomain])
              (Vertex g)
          , genInstance g'a gr'a arr
          , genInstance g'b gr'b domain
          ]

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
    , edges :>+: '[Pht Sub]
    , nodes :>+: '[T NodeBot, G, T NodeArr, T NodeTup, T NodeRec, T (NodeHas Label)]
    , nodes :>+: '[T (NodeRef name)]
    , name ~ Name, info ~ Int, ns ~ nodes
    )
instance ConstraintGen (Equation (Grp (PatSurface t)) (Prefixes name t)) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Equation _prefix br brs) = do
    -- TODO: handle local type binding
    g <- node (G 1)
    var <- node (T NodeBot)
    br'c <- handleBranch br
    br'cs <- forM brs handleBranch
    let val = (g :|) . ExprF . inj $ Equation _prefix (br'c ^. stage) (br'cs ^.. traverse . stage)
    -- unify every branch with bottom node and need to bind every sub G to top G
    grs <- forM (br'c: br'cs) \(StageConstraint _ g' gr') -> do
      r <- getInstance1 g' gr'
      return $ overlays
        [ gr'
        , r -++ T Unify ++- var
        , g' -<< T (Binding Flexible $ Nothing @name) >>- g
        ]
    -- since we duplicate graph many times, we need to compress
    -- the graph to get a minimal representation
    return $ StageConstraint val g $ overlays
      [ overlays grs
      , g -<< T (Sub 1) >>- var
      , var -<< T (Binding Flexible $ Nothing @name) >>- g
      ]
    where
      -- turn a pattern binds into unifiable bindings
      asBinding gr binds = forM binds \(name, (Instance i, g)) -> do
        r <- getInstance i g gr
        return (name, r :| Inl (T Unify))

      abstractWith (r'g, r'gr) (StageConstraint _ pat'g pat'gr) = do
        g <- node (G 1)
        arr <- node (T NodeArr)
        domain <- node (T NodeBot)
        codomain <- node (T NodeBot)
        -- gen instance
        pat'n <- getInstance1 pat'g pat'gr
        return . (g,) $ overlays
          [ r'gr, pat'gr
          -- skeleton binding edge
          , Connect (link $ T (Binding Flexible $ Nothing @name))
              (fromVertices [arr, domain, codomain]) (Vertex g)
          -- build skeleton
          , arr -<< T (Sub 1) >>- domain
          , arr -<< T (Sub 2) >>- codomain
          , g -<< T (Sub 1) >>- arr
          -- add binding edge
          , r'g -<< T (Binding Flexible $ Nothing @name) >>- g    -- body
          , pat'g -<< T (Binding Flexible $ Nothing @name) >>- g  -- pattern
          -- add constraint edge
          , r'g -<< T (Instance 1) >>- codomain -- body instance
          , domain -++ T Unify ++- pat'n        -- domain unification
          ]

      -- generate lambda with pattern bindings. Each pattern
      -- may or may not introduce new variable bindings, yet it
      -- is simply a branch of an equation group.
      handleBranch (Grp mpat mpats, mbody) = do
        -- generate first pattern
        stage'pat@(StageConstraint (PatternData pat p'binds) _ p'gr) <- genPatternConstraint mpat
        pa'bindings <- asBinding p'gr p'binds
        (stage'pats, lbindings) <- foldM stackBinding ([], pa'bindings) $ genPatternConstraint <$> mpats
        StageConstraint e e'g e'gr <- localBinding lbindings mbody
        let pats :: [(PatSurface t) (ExprF f name |: Hole nodes Int)]
            pats = reverse $ fromX . strip <$> (stage'pats ^.. traverse . stage . pattern)
            branch = (Grp (fromX . strip $ pat) pats, e)
        (g, gr) <- foldM abstractWith (e'g, e'gr) (reverse $ stage'pat:stage'pats)
        return $ StageConstraint branch g gr
          where
          -- stage constraint is built in reverse order
          stackBinding (ss, lbindings) mb = do
            s@(StageConstraint (PatternData _ pb'binds) _ pb'gr) <-
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
    , edges :>+: '[Pht Sub]
    , nodes :>+: '[T NodeBot, G, T NodeArr, T NodeTup, T NodeRec, T (NodeHas Label)]
    , nodes :>+: '[T (NodeRef name)]
    , HasNodeCreator m
    , HasConstraintGenErr name m
    , HasOrderGraph nodes edges Int
    , HasBinding name nodes m
    , LetGrp (PatSurface t) :<: f
    , name ~ Name, ns ~ nodes
    )
instance ConstraintGen (LetGrp (PatSurface t)) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LetGrp ms m) = do
    -- TODO: allow mutual recursion in let group
    envs <- forM ms \(mpat, me) -> do
      StageConstraint (PatternData pat binds) p'g p'gr <- genPatternConstraint mpat
      StageConstraint expr'stage e'g e'gr <- do
        lbinding <- asBinding p'gr binds
        localBinding lbinding me
      let pat'stage = (fromX $ strip pat) :: ((PatSurface t) (ExprF f name |: Hole nodes Int))
      p'root <- getInstance1 p'g p'gr
      e'root <- getInstance1 e'g e'gr
      return . ((pat'stage, expr'stage), binds, e'g, ) $ overlays
        [ p'gr, e'gr
        , e'root -++ T Unify ++- p'root
        , p'g -<< T (Binding Flexible $ Nothing @name) >>- e'g
        ]
    -- TODO: add duplication check for bindings
    StageConstraint expr'stage e'g e'gr <- localBinding
        [ (name, nd :| Inr (T inst))
        | binds <- envs ^.. traverse . _2
        , (name, (inst, nd)) <- binds ]
        m
    let val = ExprF . inj $ LetGrp (envs ^.. traverse . _1) expr'stage
    return $ StageConstraint (e'g :| val) e'g $ overlays
      [ e'gr, overlays $ envs ^.. traverse . _4
      -- build constraint binding edges
      , Connect (link $ T (Binding Flexible $ Nothing @name))
          (fromVertices (envs ^.. traverse . _3)) (Vertex e'g)
      ]
    where 
      -- turn a pattern binds into unifiable bindings
      asBinding gr binds = forM binds \(name, (Instance i, g)) -> do
        r <- getInstance i g gr
        return (name, r :| Inl (T Unify))

type instance ConstrainGraphic (Let (Binder (name ::: Maybe t))) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen (Let (Binder (name ::: Maybe t))) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic (Letrec (Binder (name ::: Maybe t))) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen (Letrec (Binder (name ::: Maybe t))) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic ((:::) t) (ExprF f name |: Hole ns Int) m nodes edges info = ()
instance ConstraintGen ((:::) t) (ExprF f name |: Hole nodes Int) Int where

type instance ConstrainGraphic LiteralText (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , nodes :>+: '[G, T (NodeRef name)]
    , edges :>+: '[T Sub, T (Binding name)]
    , HasNodeCreator m
    , IsString name
    , LiteralText :<: f
    , HasOrderEdge edges
    )
instance ConstraintGen LiteralText (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LiteralText (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef mempty (fromString @name "text"))
    return $ StageConstraint (g :| ExprF (inj $ LiteralText $ Literal t)) g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
      ]
type instance ConstrainGraphic LiteralInteger (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , nodes :>+: '[G, T (NodeRef name)]
    , edges :>+: '[T Sub, T (Binding name)]
    , HasNodeCreator m
    , IsString name
    , LiteralInteger :<: f
    , HasOrderEdge edges
    )
instance ConstraintGen LiteralInteger (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LiteralInteger (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef mempty (fromString @name "int"))
    return $ StageConstraint (g :| ExprF (inj $ LiteralInteger $ Literal t)) g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
      ]
type instance ConstrainGraphic LiteralNumber (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , nodes :>+: '[G, T (NodeRef name)]
    , edges :>+: '[T Sub, T (Binding name)]
    , HasNodeCreator m
    , IsString name
    , LiteralNumber :<: f
    , HasOrderEdge edges
    )
instance ConstraintGen LiteralNumber (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (LiteralNumber (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef mempty (fromString @name "double"))
    return $ StageConstraint (g :| ExprF (inj $ LiteralNumber $ Literal t)) g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
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
    , edges :>+: '[Pht Sub, T (Binding name), T Sub]
    , nodes :>+: '[T (NodeLit t), G]
    )
-- | expression literal
instance ConstraintGen (Literal t) (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Literal t) = do
    g <- node (G 1)
    n <- node (T $ NodeLit t)
    return $ StageConstraint (g :| ExprF (inj $ Literal t)) g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
      ]

-- | expression Tuple literal
type instance ConstrainGraphic Tuple (ExprF f name |: Hole ns Int) m nodes edges info
  = ( ns ~ nodes
    , Tuple :<: f
    , HasNodeCreator m
    , HasOrderGraph nodes edges info
    , edges :>+: '[Pht Sub, T (Binding name), T Sub, T Instance]
    , nodes :>+: '[G, T NodeBot, T NodeTup]
    )
-- | expression Tuple literal
instance ConstraintGen Tuple (ExprF f name |: Hole nodes Int) Int where
  stageConstraint (Tuple sma) = do
    let len = toInteger $ length sma
    g <- node (G 1)
    tup <- node (T $ NodeTup len)
    sia <- zip [1..len] <$> sequence sma
    grs <- forM sia \(ix, StageConstraint _ g' gr') -> do
      var <- node (T NodeBot)
      return $ overlays
        [ tup -<< T (Sub ix) >>- var
        , var -<< T (Binding Flexible $ Nothing @name) >>- g
        , g' -<< T (Binding Flexible $ Nothing @name) >>- g
        , g' -<< T (Instance 1) >>- var
        , gr'
        ]
    return $ StageConstraint (g :| ExprF (inj . Tuple $ sia ^.. traverse . _2 . stage)) g $ overlays
      [ overlays grs, g -<< T (Sub 1) >>- tup
      , tup -<< T (Binding Flexible $ Nothing @name) >>- g
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
     , nodes :>+: '[T NodeBot, G, T NodeArr, T NodeTup, T NodeRec, T (NodeHas label)]
     , Traversable lits, Traversable injs, HasNodeCreator m
     , HasConstraintGenErr name m
     , HasOrderGraph nodes edges Int
     )
  => Pattern lits injs label name (m (StageConstraint nodes edges Int expr))
  -> m (StageConstraint nodes edges Int target)

genPatternConstraint = cata go
  where
    functionPattern g = do
      arr <- node (T NodeArr)
      domain <- node (T NodeBot)
      codomain <- node (T NodeBot)
      return . ((arr, domain, codomain), ) $ overlays
        [ arr -<< T (Sub 1) >>- domain
        , arr -<< T (Sub 2) >>- codomain
        , Connect (link . T . Binding Flexible $ Nothing @name)
           (fromVertices [arr, domain, codomain])
           (Vertex g)
        ]

    -- runner
    go PatWildF = do
      g <- node (G 1)
      var <- node (T NodeBot)
      return $ StageConstraint (PatternData (g :| PatWildF) []) g
        $ overlays
        [ g -<< T (Sub 1) >>- var
        , var -<< T (Binding Flexible $ Nothing @name) >>- g
        ]

    go PatUnitF = do
      g <- node (G 1)
      n <- node (T $ NodeTup 0)
      return $ StageConstraint (PatternData (g :| PatUnitF) []) g
        $ overlays
        [ n -<< T (Binding Flexible $ Nothing @name) >>- g
        , g -<< T (Sub 1) >>- n
        ]

    go (PatVarF name) = do
      g <- node (G 1)
      var <- node (T NodeBot)
      return . StageConstraint (PatternData (g :| PatVarF name) [(name, (Instance 1, g))]) g
        $ overlays
        [ g -<< T (Sub 1) >>- var
        , var -<< T (Binding Flexible $ Just name) >>- g
        ]

    go (PatPrmF litm) = stageConstraint litm

    go (PatTupF sma) = do
      let len = toInteger $ length sma
      g <- node (G 1)
      tup <- node (T $ NodeTup len)
      sia <- (`zip` [1..len]) <$> sequence sma
      grs <- forM sia \(StageConstraint _ n'g n'gr, i) -> do
        var <- node (T NodeBot)
        n'root <- getInstance1 n'g n'gr
        return $ overlays
          [ n'gr
          -- add binding edge and structure edge
          , var -<< T (Binding Flexible $ Nothing @name) >>- g
          , tup -<< T (Sub i) >>- var
          -- add unify constraint
          , var -++ T Unify ++- n'root
          -- bind sub constraint
          , n'g -<< T (Binding Flexible $ Nothing @name) >>- g
          ]
      -- TODO: check redundant name binding
      let patd = PatternData (g :| PatTupF (sia ^.. traverse . _1 . stage . pattern))
                             (sia ^.. traverse . _1 . stage . bindings . traverse)
      return $ StageConstraint patd g $ overlays
        [ overlays grs
        , tup -<< T (Binding Flexible $ Nothing @name) >>- g
        , g -<< T (Sub 1) >>- tup
        ]

    -- | TODO: recheck constraint generation rule
    go (PatRecF sma) = do
      let len = toInteger $ length sma
      g <- node (G 1)
      recn <- node (T $ NodeRec len)
      sia <- (`zip` [1..len]) <$> mapM sequence sma
      grs <- forM sia \((label, StageConstraint _ n'g gr), i) -> do
        r <- getInstance1 n'g gr
        has <- node (T $ NodeHas True label)
        var <- node (T NodeBot)
        return $ overlays
          [ gr
          , Connect (link (T . Binding Flexible $ Nothing @name))
            (fromVertices [has, var, n'g]) (Vertex recn)
          , recn -<< T (Sub i) >>- has
          , has -<< T (Sub 1) >>- var
          , r -++ T Unify ++- var
          ]
      -- TODO: check redundant name binding
      let patd = PatternData (g :| PatRecF (sia ^.. traverse . _1 & traverse . _2 %~ (^. (stage . pattern))))
                             (join $ sia ^.. traverse . _1 . _2 . stage . bindings)
      return $ StageConstraint patd g $ overlays
        [ overlays grs
        , recn -<< (T . Binding Flexible $ Nothing @name) >>- g
        , g -<< T (Sub 1) >>- recn
        ]

    go (PatSymF _ _) = error "wait for whole pass being completed"
    go (PatViewF me ma) = do
      g <- node (G 1)
      -- common pattern for arrow, with domain and codomain binds to root of type
      ((arr, domain, codomain), arr'gr) <- functionPattern g
      -- expression constraint
      StageConstraint e e'g e'gr <- me
      -- pattern constraint
      StageConstraint (PatternData pat binds) pat'g pat'gr <- ma
      -- get instance of pattern G node, what we will use to unify with codomain
      pat'root <- getInstance1 pat'g pat'gr
      let patd = PatternData (g :| PatViewF e pat) binds
          gr = overlays
              [ arr'gr, pat'gr, e'gr
              -- connect result node
              , g -<< T (Sub 1) >>- domain
              -- connect right pattern node
              , pat'g -<< T (Binding Flexible $ Nothing @name) >>- g
              , pat'root -++ T Unify ++- codomain
              -- connect left expr node
              , e'g -<< T (Binding Flexible $ Nothing @name) >>- g
              , e'g -<< T (Instance 1) >>- arr
              ]
      return $ StageConstraint patd g gr

    go (PatBindF name ma) = do
      StageConstraint pati g gr <- ma
      let patd = StageConstraint $ PatternData
                 (g :| PatBindF name (pati ^. pattern))
                 ((name, (Instance 1, g)) :pati ^. bindings)
      return $ patd g gr
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
    , edges :>+: '[T (Binding name), T Sub]
    , nodes :>+: '[T (NodeRef name), G]
    , IsString name
    )
instance ConstraintGen LiteralText (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralText (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef mempty $ fromString @name "text")
    let patd = PatternData (g :| PatPrmF (inj . LiteralText $ Literal t)) mempty
    return $ StageConstraint patd g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
      ]

-- | handle type cast in pattern matching
type instance ConstrainGraphic ((:::) t) (PatternInfo lits injs ns label name expr) m nodes edges info
  = ()
instance ConstraintGen ((:::) t) (PatternInfo lits injs nodes label name expr) Int where

type instance ConstrainGraphic PatGroup (PatternInfo lits injs ns label name expr) m nodes edges info
  = ()
instance ConstraintGen PatGroup (PatternInfo lits injs nodes label name expr) Int where


-- | handle general literal
type instance ConstrainGraphic (Literal t) (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , Literal t :<: lits
    , HasOrderEdge edges
    , HasNodeCreator m
    , edges :>+: '[T (Binding name), T Sub]
    , nodes :>+: '[T (NodeLit t), G]
    )
instance ConstraintGen (Literal t) (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (Literal t) = do
    g <- node (G 1)
    n <- node (T $ NodeLit t)
    let patd = PatternData (g :| PatPrmF (inj $ Literal t)) mempty
    return $ StageConstraint patd g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
      ]

-- | handle Literal integer
type instance ConstrainGraphic LiteralInteger (PatternInfo lits injs ns label name expr) m nodes edges info
  = ( ns ~ nodes
    , LiteralInteger :<: lits
    , HasOrderEdge edges
    , HasNodeCreator m
    , edges :>+: '[T (Binding name), T Sub]
    , nodes :>+: '[T (NodeRef name), G]
    , IsString name
    )
instance ConstraintGen LiteralInteger (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralInteger (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef mempty $ fromString @name "int")
    let patd = PatternData (g :| PatPrmF (inj . LiteralInteger $ Literal t)) mempty
    return $ StageConstraint patd g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
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
    )
instance ConstraintGen LiteralNumber (PatternInfo lits injs nodes label name expr) Int where
  stageConstraint (LiteralNumber (Literal t)) = do
    g <- node (G 1)
    n <- node (T $ NodeRef mempty $ fromString @name "string")
    let patd = PatternData (g :| PatPrmF (inj $ LiteralNumber $ Literal t)) mempty
    return $ StageConstraint patd g $ overlays
      [ g -<< T (Sub 1) >>- n
      , n -<< T (Binding Flexible $ Nothing @name) >>- g
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
interiorC gr = order . dfs (isLinkOf @(T (Binding name))) (transpose gr)

-- | get structural nodes, no duplication
interiorS :: (T Sub :<: es, HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorS gr = order . dfs (isLinkOf @(T Sub)) gr

-- | get structural interior nodes, no duplication
interiorI :: forall name ns es info. (T (Binding name) :<: es, T Sub :<: es, HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
interiorI gr r = Set.toList $ Set.intersection
  (Set.fromList $ interiorC @name gr r)
  (Set.fromList $ interiorS gr r)

-- | frontier nodes and interior nodes, no duplication
frontierSI :: forall name ns es info. (es :>+: '[T (Binding name), T Sub], HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> ([Hole ns info], [Hole ns info])
frontierSI gr r =
  let sInteriors = interiorI @name gr r
   in ( Set.toList $ Set.fromList (snd <$> lFroms @(T Sub) sInteriors gr) `Set.difference` Set.fromList sInteriors
      , sInteriors)

-- | frontier nodes, no duplication
frontierS :: forall name ns es info. (es :>+: '[T (Binding name), T Sub], HasOrderGraph ns es info) =>
  CoreG ns es info -> Hole ns info -> [Hole ns info]
frontierS gr = fst . frontierSI @name gr

-- | expand a type scheme at another node and return copied type root with extracted graph.
--
-- the returned graph doesn't include original graph!!!
expand :: forall name edges nodes m .
  ( edges :>+: '[T Unify, T Instance, T (Binding name), T Instance, T Sub]
  , nodes :>+: '[T NodeBot, G] , Ord name
  , HasOrderGraph nodes edges Int , HasNodeCreator m, HasConstraintGenErr name m)
  => (G (Hole nodes Int), Int, Instance) -> Hole nodes Int -> CoreG nodes edges Int
  -- ^ (G, [(old nodes, fresh nodes)], graph)
  -> m (Hole nodes Int, CoreG nodes edges Int)
expand (g, gid, Instance inst) g't gr = do
  -- get root type of G instance, and generate its copied node
  p <- getInstance inst scheme gr
  let structureS = Set.fromList $ interiorS gr p
      interiors = Set.toList $ allInteriors `Set.intersection` structureS
      frontiers = Set.toList $ allFrontiers `Set.intersection` structureS
      -- all interiorCs bound to scheme which should be reset to expanded root type
      interiorCBinds = lTo @(T (Binding name)) scheme gr
      -- assign new node number to existed nodes to keep their structure
      -- but make them different from old ones.
      -- delete unnecessary nodes.
      refreshNode n@(Hole tag _)
        | n `elem` interiors = gets (lookup n) >>= \case
          Nothing -> do
            m <- Hole tag <$> lift newNodeCounter
            modify ((n,m):)
            return (Vertex m)
          Just m -> return (Vertex m)
        | n `elem` frontiers = gets (lookup n) >>= \case
          Nothing -> do
            m <- lift $ node (T NodeBot)
            modify ((n, m):)
            return (Vertex m)
          Just m -> return (Vertex m)
        | otherwise = return None
  -- prepare root copy manually for convenience
  p'c <- if p `elem` frontiers then node (T NodeBot) else let Hole tag _ = p in Hole tag <$> newNodeCounter
  (prune -> gr'type, pairs) <- (`runStateT` [(p, p'c)]) . unGraphT $ GraphT (return gr) >>= GraphT . refreshNode
  let typ'c =
        let -- project operation. remove instantiation link and unification link.
            proj = induceLink \e -> not $ isLinkOf @(T Unify) e || isLinkOf @(T Instance) e
            binding = Binding Flexible (Nothing @name)
            -- rebind all interiors to root of gType, excluding root node of gType.
            -- and we don't need to reset binding edge of frontier nodes since
            -- those edges are not included in the copied gType.
            reset (m, m'copy) typ
              -- nodes need to be reset have no binding edge
              | null (lFrom @(T (Binding name)) m'copy typ)
                = case lookup m interiorCBinds of
                    -- if it has an binding edge to origin scheme, it needs to be reset
                    -- and rebound to root of type
                    Just e -> typ <>
                      if m'copy == p'c -- if it is root node, root node is interier node, simply add binding edge
                         then m'copy -<< T binding >>- g't
                         else m'copy -<< e >>- p'c
                    -- otherwise, it is a frontier node and should be bound
                    -- to target G node
                    Nothing -> assert (isHoleOf @(T NodeBot) m'copy) $ overlays
                      [ m'copy -<< T binding >>- g't
                        -- add unification edge for frontier edges
                      , m'copy -++ T Unify ++- m
                      , typ
                      ]
              | otherwise = typ
        in foldr ((.) . reset) id pairs (proj gr'type)
  return (p'c, typ'c)
  where -- a `G` node may have multiple instances, so what we get here are
        -- nodes among all of its instances
        (allFrontiers, allInteriors) = bimap Set.fromList Set.fromList $ frontierSI @name gr scheme
        -- rebuild G node here
        scheme = hole g gid

-- | propagate a graphic constraint. return roots of propagated type.
propagate :: forall name edges nodes err m
  . ( edges :>+: '[T Unify, T Instance, T (Binding name), T Sub]
    , T NodeBot :<: nodes, G :<: nodes
    , Ord name , HasOrderGraph nodes edges Int
    , HasNodeCreator m, HasConstraintGenErr name m, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m, Show (nodes (Hole nodes Int)))
  => (G (Hole nodes Int), Int) -> CoreG nodes edges Int
  -> m ([Hole nodes Int], CoreG nodes edges Int)
propagate (g, gid) gr = do
  (unzip -> (ns, grs'c)) <- forM constraints \(inst, n) -> do
    binder <- case lFrom @(T (Binding name)) n gr of
      [(_, binder)] -> return binder
      [] -> failSolvProp gr $ CSPropMultipleBinder n [] "A node has no binders during propagation"
      binders -> failSolvProp gr
        $ CSPropMultipleBinder n (snd <$> binders) "A node has multiple binders during propagation"
    (!r'c, !gr'c) <- expand (g, gid, inst) binder gr
    return (r'c, gr'c <> (r'c -++ T Unify ++- n))
  return (ns, mconcat (gr:grs'c))
  where
    scheme = hole g gid
    constraints = lFrom scheme gr <&> \(T (Instance i), n) -> (Instance i, n)

type HasUnifier err nodes edges info m =
  HasReader "Unifier" (Unifier err nodes edges info m) m
type Unifier err nodes edges info m
  = CoreG nodes edges info
  -> Hole nodes info -> Hole nodes info
  -> m (Either err (Hole nodes info, CoreG nodes edges info))

askUnifier :: HasUnifier err nodes edges info m => m (Unifier err nodes edges info m)
askUnifier = ask @"Unifier"

-- | solve all `Unify` edges in a stage constraint
solvUnify
  ::  ( HasUnifier err nodes edges info m, edges :>+: '[T Unify]
      , HasOrderGraph nodes edges info, HasSolvErr (Hole nodes Int) (CoreG nodes edges info) err m)
  => CoreG nodes edges info -> m (CoreG nodes edges info)
solvUnify (compress -> graph) = do
  let unification = order $ join [[from, to] | (e, from, to) <- toEdges graph, isLinkOf @(T Unify) e && from /= to]
  resolve unification graph
  where
    resolve [] gr = return gr
    resolve (a:as) gr = do
      (snd -> gr') <- foldM unify (a, gr) . order . filter (/= a) $ snd <$> lFrom @(T Unify) a gr
      resolve as gr'
      where unify (n, gr') n'
              | n' `notElem` toVertices gr' = return (n, gr')
              | otherwise = do
                unifier <- askUnifier
                unifier gr' n n' >>= \case
                  Left err -> failSolvUnify err
                  Right ret -> return ret

-- | solve a `Instance` edge
solvInst
  :: forall name nodes edges m err
  . ( HasUnifier err nodes edges Int m, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m
    , HasConstraintGenErr name m , Ord name , HasNodeCreator m
    , nodes :>+: '[G, T NodeBot]
    , edges :>+: '[Pht Sub, T (Binding name), T Unify, T Instance, T Sub]
    , HasOrderGraph nodes edges Int, Show (nodes (Hole nodes Int)) )
  => (Hole nodes Int, CoreG nodes edges Int)
  -> m (CoreG nodes edges Int)
solvInst (r, graph) = do
  -- TOOD: verify it is acyclic graphic constraint
  let depencency =
        let pick _ (_, ins) = [ n| ((n, e), _) <- ins, isLinkOf @(T (Binding name)) e, isHoleOf @G n]
         in reverse $ topSort (bfs (const [r]) pick) pick graph
  go graph depencency
  where
    go gr [] = return gr
    go gr (Hole tag gid:ns) =
      case prj @G tag of
        Just g@(G _) -> do
          (_, gr') <- propagate @name (g, gid) gr
          gr'' <- solvUnify gr'
          go gr'' ns
        Nothing -> error "Expect G node"

-- | it accepts a staged constraint and return a presolution
solveConstraint
  :: forall name nodes edges m err a
  . ( HasUnifier err nodes edges Int m
    , HasNodeCreator m, HasConstraintGenErr name m
    , Ord name
    , edges :>+: '[Pht Sub, T (Binding name), T Unify, T Instance, T Sub]
    , nodes :>+: '[G, T NodeBot]
    , HasOrderGraph nodes edges Int, HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m, Show (nodes (Hole nodes Int)) )
  => StageConstraint nodes edges Int a -> m (StageConstraint nodes edges Int a)
solveConstraint sc@(StageConstraint _ r graph) = do
  gr <- solvUnify graph
  gr' <- solvInst @name (r, gr)
  return $ (storage .~ gr') sc
{-# INLINE solveConstraint #-}

-- | get a graphic type from a `G` node of a presolution.
getSolution
  :: forall name m nodes edges err. ( HasNodeCreator m
     , HasConstraintGenErr name m, HasUnifier err nodes edges Int m
     , HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m
     , Ord name
     , nodes :>+: '[NodePht, T NodeBot, G]
     , edges :>+: '[T (Binding name), T Unify, T Instance, T Sub]
     , HasOrderGraph nodes edges Int
     )
  => (Hole nodes Int, Instance) -> CoreG nodes edges Int
  -> m (Hole nodes Int, CoreG nodes edges Int)
getSolution (scheme@(Hole tag gid), inst) graph = do
  case prj @G tag of
    Just g@(G _) -> do
      (r, gr) <- expand (g, gid, inst) scheme graph
      gr' <- solvUnify (graph <> gr)
      return (r, gr')
    Nothing -> error "Expect G node"

-- | get a solution from a constraint presolution
getSolutionFromConstraint
  :: ( HasNodeCreator m
     , Ord name
     , HasConstraintGenErr name m, HasUnifier err nodes edges Int m
     , HasSolvErr (Hole nodes Int) (CoreG nodes edges Int) err m
     , nodes :>+: '[NodePht, T NodeBot, G]
     , edges :>+: '[T (Binding name), T Unify, T Instance, T Sub]
     , HasOrderGraph nodes edges Int
     )
  => StageConstraint nodes edges Int a
  -> m (Hole nodes Int, CoreG nodes edges Int)
getSolutionFromConstraint (StageConstraint _ g graph)
  = getSolution (g, Instance 1) graph
