module Tlang.Inference.Expr
   (
      -- genConstraint
   )
where

import Tlang.AST
import Tlang.Extension.Decl
import Tlang.Graph.Core
import Tlang.Unification.Type
import Tlang.Graph.Extension.Type
import Tlang.Generic ((:<:) (..), (:+:) (..))

import Capability.State (HasState, get, modify)
import Capability.Reader (HasReader, ask, asks)
import Capability.Error (HasThrow, throw)
import Data.Functor.Foldable (cata)
import Data.Functor ((<&>), ($>))

data With f a = With (f a) a deriving (Show, Eq, Ord, Functor)

data ExprError
   = FailMsg String
   deriving (Show, Eq, Ord)

failExprMsg :: HasThrow "fail" ExprError m => String -> m a
failExprMsg = throw @"fail" . FailMsg

data Mode a
   = Restrict a | Polymorphic a deriving (Show, Eq, Ord, Functor)

unMode :: Mode a -> a
unMode (Restrict a) = a
unMode (Polymorphic a) = a

-- genConstraint
--   :: ( HasReader "decl" (Decl ds info) m
--      , HasState "node" Int m
--      , HasThrow "fail" ExprError m
--      , HasReader "local" [(name, (With (Uno Unify) :+: With (Uno Instance)) (Mode (Hole ns Int)))] m
--      , Show name, Eq name
--      , Ord (es (Link es))
--      , Uno NodeBot :<: ns, Uno G :<: ns
--      , Uno (NodeLit String) :<: ns, Uno (NodeLit Integer) :<: ns, Uno (NodeLit Double) :<: ns
--      , Uno NodeTup :<: ns
--      , Uno Sub :<: es, Uno (Bind name) :<: es, Uno Instance :<: es, Uno Unify :<: es
--      )
--   => Expr typ ((:@) typ) name
--   -> m (Expr typ ((:@) (Hole ns Int)) name, CoreG ns es Int, Hole ns Int)
-- genConstraint = cata go
--    where
--      node = modify @"node" (+1) >> get @"node"
--      go (ExUnitF) = node <&> hole' (Uno $ NodeTup 0) <&> \v -> (ExUnit, Vertex v, v)
--      go (ExLitF n@(LitInt v)) = node <&> hole' (Uno $ NodeLit v) <&> \v -> (ExLit n, Vertex v, v)
--      go (ExLitF n@(LitNumber v)) = node <&> hole' (Uno $ NodeLit v) <&> \v -> (ExLit n, Vertex v, v)
--      go (ExLitF n@(LitString v)) = node <&> hole' (Uno $ NodeLit v) <&> \v -> (ExLit n, Vertex v, v)
--      go (ExRefF name) = do
--         val'maybe <- asks @"local" (lookup name)
--         gnode <- node <&> hole' (Uno $ G 1)
--         case val'maybe of
--           Just (Inl (With _ (unMode -> n))) -> return (ExRef name, Vertex n, n)
--           Just (Inr (With (Uno (Instance i)) (unMode -> n))) -> do
--              v <- node <&> hole' (Uno NodeBot)
--              return (ExRef name, overlays [n -<< Uno (Instance i) >>- v, v -<< (Uno $ Bind Flexible 1 (Just name)) >>- gnode, gnode -<< Uno (Sub 1) >>- v], gnode)
--           Nothing -> failExprMsg $ "name not in scope: " <> show name
--      go (ExSelF _) = failExprMsg "unsupported field selector"
--      go (ExTypF typ) = failExprMsg "unsupported type application"
--      go (ExTupF ns) = undefined

-- solveConstraint

-- infer raw = do
--   (expr, constraint) <- genConstraint raw
--   g <- solveConstraint constraint
--   getResult expr g
