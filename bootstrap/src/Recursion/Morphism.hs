module Recursion.Morphism
  ( dyna
  , mutu
  )
where


import Data.Functor.Foldable
import Control.Comonad.Cofree
import Control.Comonad

-- | dynamorphism scheme for recursion
dyna :: Functor f => (f (Cofree f a) -> a) -> (c -> f c) -> c -> a
dyna alg coalg = extract . hylo (\x -> alg x :< x) coalg

-- | mutumorphism scheme for recursion
mutu :: Recursive t => (Base t (a, b) -> a) -> (Base t (a, b) -> b) -> (t -> a, t -> b)
mutu alg1 alg2 = (fst . h, snd . h)
  where alg x = (alg1 x, alg2 x)
        h = cata alg