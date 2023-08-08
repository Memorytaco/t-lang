module Utility
  (
    Pretty (..)
  )
where


class Pretty a where
  pretty :: Monad m => a -> m String
