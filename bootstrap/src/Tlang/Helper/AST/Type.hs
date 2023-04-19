module Tlang.Helper.AST.Type
  ( getMonoType
  )
where

import Tlang.AST.Type

import Data.Bifunctor (first)

getMonoType :: Traversable f
            => Type label name lit bind c f rep
            -> ([bind (Type label name lit bind c f rep)], Type label name lit bind c f rep)
getMonoType (TypAll b1 t) = first (b1:) $ getMonoType t
getMonoType (TypNest fa) = TypNest <$> mapM getMonoType fa
getMonoType t = ([], t)

