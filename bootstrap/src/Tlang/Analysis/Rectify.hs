module Tlang.Analysis.Rectify

where


import Tlang.AST
import Data.Functor.Foldable
import Control.Monad (MonadFail (..))

type UnTypedModule = Module String String (Declaration Op (TypName Op) (UnTypedName Op)) Op (TypName Op) (UnTypedName Op)

type KindedType k = Type (KindedName (TypeKind k) (Either Op String)) ()

kindApply :: Eq k => TypeKind k -> TypeKind k -> Maybe (TypeKind k)
kindApply (KindCat k1 r) k2 = if k1 == k2 || k1 == KindAny then Just r else Nothing
kindApply _ _ = Nothing

getNameTypeKind :: (Eq k, Show k) => [(Either Op String, KindedType k)] -> Either Op String -> Maybe (TypeKind k)
getNameTypeKind env name = do
  typ <- lookup name env
  cata go typ
  where
    getKind :: KindedName k s -> k
    getKind (TermCons k _) = k
    getKind (TypSym k _ _) = k
    getKind (TypBound k _ _) = k

    go :: (Show k, Eq k) => Base (KindedType k) (Maybe (TypeKind k)) -> Maybe (TypeKind k)
    go (TypRefF (getKind -> k)) = pure k
    go (TypAllF (getKind -> k) _) = pure k
    go (TypAbsF (getKind -> k1) k2) = KindCat k1 <$> k2
    go (TypAppF mk1 mk2) = do
      k1 <- mk1
      k2 <- mk2
      let msg = error $ "fatal error: when trying doing type application, find mismatched type kind of " <> show k1 <> " " <> show k2
      maybe msg pure $ kindApply k1 k2
    go _ = Just KindType

inferKind :: forall m k. (Monad m, MonadFail m)
          => [(Either Op String, KindedType k)] -- ^ global environment, with kinded type
          -> Either Op String                 -- ^ name of the type, for detecting recursive
          -> [(Either Op String, Maybe (TypeKind KindVal))] -- ^ type variable or quantified type
          -> Type (TypName Op) () -> m (KindedType k)
inferKind env _ vars tree = cata go tree
  where
    go :: Base (Type (TypName Op) ()) (m (KindedType k)) -> m (KindedType k)
    go TypBottomF = return TypBottom
    go TypUnitF = return TypUnit
    go (TypRepF r) = return $ TypRep r
    go _ = undefined

