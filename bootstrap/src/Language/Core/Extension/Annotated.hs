module Language.Core.Extension.Annotated
  ( SourceRange (..)
  )
where

-- | Annotation for source parsing.
--
-- It records position and file context of a
-- syntax element.
data SourceRange loc a =
  SourceRange
    { sourceStart :: loc
    , sourceEnd :: loc
    , source :: a
    } deriving (Show, Eq, Functor, Foldable, Traversable)


