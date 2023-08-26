module Compiler.TypeChecking
  (
    tcExprToGraphciType
  , tcExprToSyntacticType
  , TypeCheckingErr (..)
  )
where


import qualified Driver.GraphicConstraint as DGC
import Driver.Unification (unify, GraphUnifyError)

import Language.Core (ExprF, Name, Label, ExprSurface, ExprSurfaceExt, TypSurface)
import Graph.Core (Hole, CoreG)
import Graph.Extension.GraphicType
import Language.Generic
import Language.Constraint.Graphic
import Data.Text (Text)
import Driver.Transform.GraphType (runGraphType, syntacticType)
import Driver.Transform (UnifyGNodes, UnifyGEdges)
import Driver.GraphicConstraint (SolverErr)
import Transform.GraphType (GraphToTypeErr)

data TypeCheckingErr
  = TCSolverErr (SolverErr Name (GraphUnifyError (Hole UnifyGNodes Int)))
  | TCTransForm2Typ (GraphToTypeErr (Hole UnifyGNodes Int))
  deriving (Show, Eq)

tcExprToGraphciType
  :: forall nodes edges m target
   . ( target ~ (ExprF (ExprSurfaceExt TypSurface) Name |: Hole nodes Int)
     , ConstrainGraphic (ExprSurfaceExt TypSurface) target (DGC.GCGen Name nodes m) nodes edges Int
     , edges :>+: '[Pht O, Pht Sub, Pht NDOrderLink, T (Binding Name), T Unify, T Instance, T Sub]
     , nodes :>+: '[NDOrder, G, T NodeBot, NodePht, T NodeSum, T NodeRec, Histo, T (NodeLit Integer), T (NodeLit Text), T (NodeHas Label)]
     , Monad m
     )
  => BindingTable Name nodes -> Int
  -> ExprSurface TypSurface
  -> m (Either (DGC.SolverErr Name (GraphUnifyError (Hole nodes Int))) ((Hole nodes Int, CoreG nodes edges Int), Int))
tcExprToGraphciType env i = DGC.infer env i unify

tcExprToSyntacticType
  :: Monad m
  => BindingTable Name UnifyGNodes
  -> Int -> (String, Int)
  -> ExprSurface TypSurface
  -> m (Either TypeCheckingErr (TypSurface, Int))
tcExprToSyntacticType env i s e =
  tcExprToGraphciType @UnifyGNodes @UnifyGEdges env i e >>= \case
  Left err -> return $ Left $ TCSolverErr err
  Right (source, i') -> transfromType s source >>= \case
    Left err -> return $ Left $ TCTransForm2Typ err
    Right (t :: TypSurface, _) -> return $ Right (t, i')
  where
    transfromType x (g, gr) = runGraphType gr [] x syntacticType g
