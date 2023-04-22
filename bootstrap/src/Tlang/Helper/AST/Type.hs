module Tlang.Helper.AST.Type
  ( getMonoType
  , getTypeLit
  , injTypeLit
  , injTypeBind
  )
where

import Tlang.AST.Type
import Tlang.Extension.Type
import Tlang.Generic

import Data.Bifunctor (first)

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
