{- | name checking stage
--
-- this module provides functionality of converting raw surface module into
-- wellformed surface module, which can be used as inputs in the next compiler pipeline.
--
-- generally speaking, we will check validity of names in source code and rename them to
-- avoid name collision.
-}
module Compiler.NameChecking
  (
    NamePrefix (..)
  )
where

data NamePrefix
  -- | variable in type level
  = NameTypeVariable
  -- | variable in term level, consumed
  | NameTermVariable
  -- | variable in pattern, introduced
  | NameBindingVariable