module Tlang.AST.Operator
  ( termOperator
  , typOperator
  , getBindPower
  , withOperator
  , matchPairOperator

  , Operator (..)
  , OperatorKind (..)
  , OperatorClass (..)
  )
where

import Data.Bifunctor (Bifunctor (..))

data Operator a = Operator OperatorKind Integer Integer a deriving stock (Eq, Functor)
data OperatorKind = Prefix | Postfix | Unifix | Infix deriving (Show, Eq)

data OperatorClass s v = OpRep (Either (Either (s, s) (s, s)) s)
                       | OpNorm v
                       deriving (Show, Eq, Ord, Functor)

instance Bifunctor OperatorClass where
  bimap fa _ (OpRep r) = OpRep $ bimap (bimap (bimap fa fa) (bimap fa fa)) fa r
  bimap _ fb (OpNorm b) = OpNorm $ fb b

withOperator :: (v -> a) -> (Either (Either (s, s) (s, s)) s -> a) -> OperatorClass s v -> a
withOperator f _ (OpNorm v) = f v
withOperator _ f (OpRep v) = f v

matchPairOperator :: Eq s => Either (s, s) (s, s) -> OperatorClass s v -> a -> a -> a
matchPairOperator _ (OpNorm _) _ f = f
matchPairOperator _ (OpRep (Right _)) _ f = f
matchPairOperator v (OpRep (Left s)) t f = if v == s then t else f

getBindPower :: Operator a -> (Integer, Integer)
getBindPower (Operator _ l r _) = (l, r)

instance {-# InCoherent #-} Show (Operator String) where
  show (Operator _ _ _ s) = s
instance Show s => Show (Operator s) where
  show (Operator _ _ _ s) = show s

-- | builtin term level operator
termOperator :: [Operator String]
termOperator = []

-- | builtin type level operator
typOperator :: [Operator String]
typOperator =
  [ Operator Infix   0   (-5)   "*"
  , Operator Infix (-10) (-15)  "->"
  , Operator Infix (-10) (-15)  "."
  ]
