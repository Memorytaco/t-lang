module Tlang.Helper.AST.Type
  ( getMonoType
  , getTypeLit
  , injTypeLit
  , injTypeBind
  , elimTypEff
  )
where

import Tlang.AST.Type
import Tlang.Extension.Type
import Tlang.Generic

import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (..))
import Data.Functor.Foldable

getMonoType
  :: (Traversable inj, Forall b :<: bind)
  => Type name cons bind inj rep
  -> ([b (Type name cons bind inj rep)], Type name cons bind inj rep)
getMonoType body@(TypLet binder t) =
  case prj binder of
    Just (Forall b) -> first (b:) $ getMonoType t
    Nothing -> pure body
getMonoType a = pure a

getTypeLit
  :: forall lit name cons bind inj rep. lit :<: cons
  => Type name cons bind inj rep
  -> Maybe (lit (Type name cons bind inj rep))
getTypeLit (TypLit lit) = prj lit
getTypeLit _ = Nothing

injTypeLit
  :: lit :<: cons
  => lit (Type name cons bind inj rep)
  -> Type name cons bind inj rep
injTypeLit lit = TypLit (inj lit)

injTypeBind
  :: binder :<: bind
  => binder (Type name cons bind inj rep)
  -> Type name cons bind inj rep
  -> Type name cons bind inj rep
injTypeBind binder = TypLet (inj binder)

matchBinder
  :: binder :<: bind
  => Type name cons bind inj rep
  -> Maybe (binder (Type name cons bind inj rep), Type name cons bind inj rep)
matchBinder (TypLet binder body) = (,body) <$> prj binder
matchBinder _ = Nothing

-- | define effects cast on type
class Functor inj => TypeEff inj where
  typEff :: TypeEff f => inj (Type name cons bind f rep) -> Type name cons bind f rep

instance (TypeEff eff1, TypeEff eff2) => TypeEff (eff1 :+: eff2) where
  typEff (Inl a) = typEff a
  typEff (Inr a) = typEff a

instance TypeEff Identity where
  typEff (Identity t) = t

elimTypEff
  :: forall any eff bind cons name a. (TypeEff eff, TypeEff any, Traversable any, Traversable eff, Traversable bind, Traversable cons)
  => Type name cons bind eff a
  -> Type name cons bind any a
elimTypEff = cata \case
  TypPhtF -> TypPht
  TypRepF r -> TypRep r
  TypRefF n -> TypRef n
  TypLitF fr -> TypLit fr
  TypConF r rs -> TypCon r rs
  TypLetF fr r -> TypLet fr r
  TypInjF eff -> elimTypEff $ typEff eff
