module Utility.Data
  ( Snd (..)

  )
where

newtype Snd a b = Snd (a, b) deriving (Show, Functor, Foldable, Traversable)
instance Eq b => Eq (Snd a b) where
  Snd (_, a) == Snd (_, b) = a == b
instance Ord b => Ord (Snd a b) where
  compare (Snd (_, a)) (Snd (_, b)) = compare a b

newtype Fst b a = Fst (a, b) deriving (Show, Functor, Foldable, Traversable)
instance Eq a => Eq (Fst b a) where
  Fst (a, _) == Fst (b, _) = a == b
instance Ord a => Ord (Fst b a) where
  compare (Fst (a, _)) (Fst (b, _)) = compare a b