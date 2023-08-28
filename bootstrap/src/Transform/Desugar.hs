{-# LANGUAGE QuantifiedConstraints #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
module Transform.Desugar
  ( desugarType
  )
where

import Language.Core (Type (..), TypeF (..), type (+>) (..))
import Language.Core.Extension (Forall (..))
import Language.Core.Constraint
import Language.Generic 

desugarType
  :: forall name bind rep a
   . ( Functor rep, Functor bind
     , Forall (Prefix name) :<: bind
     , forall x. Eq x => Eq (bind x), forall x. Eq x => Eq (rep x)
     , Eq name, Eq a
     )
  => Type bind rep name a -> Type bind rep name a
desugarType = cata go
  where
    go TypPhtF = TypPht
    go (TypVarF a) = TypVar a
    go (TypConF a as) = TypCon a as
    go (TypBndF bnd a) =
      case prj @(Forall (Prefix name)) bnd >>= withPrefix of
        Just (_name, typ) -> desugarType $ a >>= \case
          Bind _ -> typ
          Free t -> t
        Nothing -> TypBnd bnd a
      where
        withPrefix (Forall ((name :: name) :~ typ)) =
          if typ == TypPht then Nothing else Just (name, typ)
        withPrefix (Forall (name :> typ)) = if typ == TypPht then Nothing else Just (name, typ)
    go (TypeF f) = Type f