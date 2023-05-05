module Tlang.TH
  ( fixQ
  )
where

import Language.Haskell.TH (Q)


-- | Why need this?
-- see https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0246-overloaded-bracket.rst
-- and https://github.com/recursion-schemes/recursion-schemes/pull/132
fixQ :: Q a -> Q a
fixQ = id
