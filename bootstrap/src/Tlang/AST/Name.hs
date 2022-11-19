module Tlang.AST.Name
  ( UntypedName (..)
  , TypedName (..)

  , getTypedNameT
  )
where


-- name reference, we don't know what type this name is, so simply records it.
data UntypedName =
    UntypedName String
  | UntypedMarker deriving (Show, Eq)

-- Another Wrapper to help analyze expression
data TypedName a = TypedName String a
                 | TypedOnly a
                 deriving (Eq, Functor)

instance Show a => Show (TypedName a) where
  show (TypedName n a) = n <> ": " <> show a
  show (TypedOnly a) = show a

getTypedNameT :: TypedName a -> a
getTypedNameT (TypedName _ a) = a
getTypedNameT (TypedOnly a) = a

