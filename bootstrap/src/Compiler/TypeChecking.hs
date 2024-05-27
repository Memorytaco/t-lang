{- | TypeChecking module
--
-- Place TypeChecking related definitions here.
---}
module Compiler.TypeChecking
  (
    tcExprToGraphciType
  , tcExprToSyntacticType
  , TypeCheckingErr (..)
  )
where


import qualified Driver.Constraint as DGC
import Driver.Constraint (SolverErr)
import Driver.Transform.GraphType (runGraphType, syntacticType)

import Driver.Unification (unify, GraphUnifyError)

import Language.Core (Name, ExprSurface, TypSurface, ConstraintSurface, ConstraintNodesSurface, ConstraintEdgesSurface)
import Graph.Core (Hole, CoreG)
import Language.Constraint.Graphic
import Transform.GraphType (GraphToTypeErr)

data TypeCheckingErr
  = TCSolverErr (SolverErr Name (Hole ConstraintNodesSurface Int) ConstraintSurface (GraphUnifyError (Hole ConstraintNodesSurface Int)))
  | TCToSyntacticTyp (GraphToTypeErr (Hole ConstraintNodesSurface Int))
  deriving (Show, Eq)

tcExprToGraphciType
  :: forall nodes edges m . (nodes ~ ConstraintNodesSurface , edges ~ ConstraintEdgesSurface , Monad m)
  => BindingTable Name nodes -> Int
  -> ExprSurface TypSurface
  -> m (Either (DGC.SolverErr Name (Hole nodes Int) (CoreG nodes edges Int) (GraphUnifyError (Hole nodes Int))) ((Hole nodes Int, CoreG nodes edges Int), Int))
tcExprToGraphciType env i = DGC.infer env i unify

tcExprToSyntacticType
  :: Monad m
  => BindingTable Name ConstraintNodesSurface
  -> Int -> (String, Int)
  -> ExprSurface TypSurface
  -> m (Either TypeCheckingErr (TypSurface, Int))
tcExprToSyntacticType env i hint e =
  tcExprToGraphciType env i e >>= \case
  Left err -> return . Left $ TCSolverErr err
  Right (source, i') -> transfromType source >>= \case
    Left err -> return . Left $ TCToSyntacticTyp err
    Right (t :: TypSurface, _) -> return $ Right (t, i')
  where
    transfromType (g, gr) = runGraphType gr [] hint syntacticType g
