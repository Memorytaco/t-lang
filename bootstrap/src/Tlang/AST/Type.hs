module Tlang.AST.Type
  ( TypAnno (..)
  , TypAnnoF (..)
  )
where

import Data.Functor.Foldable.TH

-- a simple type annotation
data TypAnno = TypName String   -- simple name for type name reference, to be resolved in next pass
             | TypPtr TypAnno   -- simple pointer type
             | TypArrow TypAnno TypAnno -- function type
             | TypArray TypAnno (Maybe Integer) -- array type
             | TypApply TypAnno TypAnno -- type application, TODO: this is not supported now!!
             | TypRec String [(String, TypAnno)]  -- Record type declaration
             | TypSum String [(String, TypAnno)]  -- Variant type declaration
             deriving (Show, Eq)

$(makeBaseFunctor ''TypAnno)
