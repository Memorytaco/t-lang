module Language.AST
  ( k1
  )
where


import Tlang.AST.Type

import Data.Functor.Identity (Identity (..))

k1 :: Kind f String (String >| String)
k1 = KindVar (New "Hello")
