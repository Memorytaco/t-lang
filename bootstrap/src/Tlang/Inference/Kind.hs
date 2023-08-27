{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Tlang.Inference.Kind
  (
    KindUnifyError (..)
  , shift
  , clean

  )
where

import Language.Core
import Language.Generic (type (|:) (..))

import qualified Data.Functor.Foldable as F (ListF (..))
import Data.Functor.Foldable hiding (ListF (..))
import Control.Monad (join)

import Capability.Reader (HasReader)
import Capability.State (HasState, get, modify)
import Capability.Error (HasThrow, throw)

-- | data type used to hold annotated `Type`
newtype AnnotatedType bind rep name a anno = AnnotatedType (Base (Type bind rep name a) |: anno)
  deriving (Functor, Foldable, Traversable)

shift :: (Eq a, Monad m1, Monad m2) => a -> m1 a -> m1 (a +> m2 a)
shift val term = do
  var <- term
  if var == val
     then return (Bind var)
     else return . Free $ return var

-- | take a list of variables, generalise the kind, add binder on top
generalize :: (Functor f, Eq a) => [a] -> Kind f a a -> Kind f a a
generalize = flip $ foldl $ \ kind a -> KindBnd a (shift a kind)

-- | take an unknown kind term, return a closed term
close :: Traversable f => Kind f info a -> Maybe (Kind f info b)
close = traverse (const Nothing)

-- | substitute one variable with term, shift down index
substitute
  :: (Functor f, Eq a)
  => a -> Kind f name a -> Kind f name a
  -> Kind f name a
substitute var target term = term >>= \a -> if a == var then target else return a

-- | remove redundant binder
clean :: Traversable f => Kind f name a -> Kind f name a
clean (Kind v) = Kind (clean <$> v)
clean (a :-> b) = clean a :-> clean b
clean (KindBnd i val) =
  let iter (Bind _) = Nothing
      iter (Free v) = Just v
   in case traverse iter val of
        Just mval -> clean (join mval)
        Nothing -> KindBnd i (fmap clean <$> val)
clean a = a

data KindUnifyError k
  = KindMismatch k k
  | InvalidPolyKind
  deriving (Show, Eq, Ord)

failUnify :: HasThrow KindUnifyError (KindUnifyError k) m => KindUnifyError k -> m a
failUnify = throw @KindUnifyError

mkGraft :: (Functor f, Eq a) => a :>> Kind f name a -> Kind f name a -> Kind f name a
mkGraft (a :>> b) = substitute a b

unify :: (HasThrow KindUnifyError (KindUnifyError (Kind f name a)) m, Eq a, Functor f)
      => [Kind f name a :<> Kind f name a]
      -> m [a :>> Kind f name a]
unify = refold shrink rollup
  where
    rollup [] = F.Nil
    rollup ((a1 :-> b1) :<> (a2 :-> b2) :xs) = rollup $ [a1 :<> a2, b1 :<> b2] <> xs
    rollup (x :xs) = F.Cons x xs

    -- solve :: (Kind f name (a >| name) :<> Kind f name (a >| name))
    --       -> m [name :>> Kind f name (a >| name)]
    solve (KindVar v :<> k@(KindVar v2)) = if v == v2 then return [] else return [v :>> k]
    solve (KindVar v :<> k) = return [v :>> k]
    solve (k :<> KindVar v) = return [v :>> k]

    solve (KindStar :<> KindStar) = pure []
    solve (k1@KindStar :<> k2) = failUnify $ KindMismatch k1 k2
    solve (k1 :<> k2@KindStar) = failUnify $ KindMismatch k1 k2

    solve (KindBnd _ _ :<> _) = failUnify InvalidPolyKind
    solve (_ :<> KindBnd _ _) = failUnify InvalidPolyKind

    solve ((a1 :-> b1) :<> (a2 :-> b2)) = do
      r1 <- solve (a1 :<> a2)
      r2 <- solve (b1 :<> b2)
      return $ r1 <> r2

    shrink F.Nil = pure []
    shrink (F.Cons (k1 :<> k2) ts) = do
      ls <- ts
      let graft = foldr (.) id $ mkGraft <$> ls
      a <- solve (graft k1 :<> graft k2)
      return $ a <> ls

-- | kinding problem with name
-- failure example:
-- data F f g a = (f g * f a * g a) ;;

genConstraint
  :: ( MonadFail m
     , HasState "name" Integer m
     , HasState "env" [Kind f String String @: name] m  -- name with unsolved kind, kind could be open
     , HasReader "env" [Kind f String solved @: name] m -- name with solved kind, kind should be closed
     , HasReader "local" [Kind f String String @: name] m -- local bound
     , HasState "constraint" [Kind f String String :<> Kind f String String] m -- generated constraint
     , Functor f, Functor rep, Functor bind
     )
  => Type bind rep name a
  -> m (AnnotatedType bind rep (Kind f String String @: name) a (Kind f String String))
genConstraint = cata go
  where
    -- | new meta kind variable
    freshName = do
      modify @"name" (+1)
      num <- get @"name"
      return $ "$" <> (show num)
    annotate typ a = AnnotatedType (a :| typ)

    go TypPhtF = return $ TypPhtF `annotate` KindStar
    -- go (TypPrmF litr) = do
    --   let handler = handleTup <$> (prj litr)
    --             <|> handleRec <$> (prj litr)
    --             <|> handleSum <$> (prj litr)
    --             <|> handleLitNat <$> (prj litr)
    --             <|> handleLitText <$> (prj litr)
    --   case handler of
    --     Just m -> m
    --     Nothing -> fail $ "Unknown literal"
    --   where
    --     handleTup (Tuple rm) = do
    --       (s, rs) <- foldl (\(ss, ts) (as, term :@ k) -> (ss <> (KindType :<> k : as), ts <> [term])) mempty <$> sequence rm
    --       return (s, injTypeLit @Tuple (Tuple rs) :@ KindType)
    --     handleRec (Record rm) = do
    --       (s, rs) <- foldl (\(ss, ts) (l, (s, term :@ k)) -> (ss <> (k :<> KindType : s), ts <> [(l,term)])) mempty <$> mapM sequence rm
    --       return (s, injTypeLit @(Record Label) (Record rs) :@ KindType)
    --     handleSum (Variant rm) = do
    --       (s, rs) <- foldl collectField mempty <$> forM rm (mapM sequence)
    --       return (s, injTypeLit @(Variant Label) (Variant rs) :@ KindType)
    --         where
    --           collectField (ss, ts) (l, Nothing) = (ss, ts <> [(l, Nothing)])
    --           collectField (ss, ts) (l, Just (s, t :@ k)) = (ss <> (k :<> KindType : s), ts <> [(l, Just t)])
    --     -- FIXME: add kind literal
    --     handleLitNat (LiteralNatural (getLiteral -> v)) = return . pure $ injTypeLit @Ext.LiteralNatural (LiteralNatural $ Literal v) `annotate` KindType
    --     handleLitText (LiteralText (getLiteral -> v)) = return . pure $ injTypeLit @Ext.LiteralText (LiteralText $ Literal v) `annotate` KindType

    -- go (TypBndF binder mt) = do
    --   let handler = handleAbs <$> (prj binder) <|> handleAll <$> (prj binder)
    --   case handler of
    --     Just m -> m
    --     Nothing -> fail "Unknown binder"
    --   where
    --     handleAbs (Scope nm) = do
    --       n <- freshName
    --       (s, name) <- mapM (fmap \(t :@ _) -> t) <$> stack (sequence nm) n
    --       (subst, term :@ k) <- stack mt n
    --       return (s <> subst, injTypeBind @(Scope (Bound Integer)) (Scope name) term `annotate` (n ::> k))
    --     handleAll (Forall nm) = do
    --       n <- freshName
    --       (s, name) <- mapM (fmap \(t :@ _) -> t) <$> stack (sequence nm) n
    --       (subst, term :@ k) <- stack mt n
    --       return (s <> (k :<> KindType : subst), injTypeBind @(Forall (Bound Integer)) (Forall name) term `annotate` KindType)

    go (TypVarF var) = do
      name <- freshName
      undefined
      -- item <- gets $ (!! fromInteger (i - 1)) . fst
      -- return . pure $ TypVar v `annotate` item

    {- single recursive type is explicitly marked since it doesn't complicate the kind inference process.
    -- same technique used as for mutual recursive type.
    -- a single recursive type is only possible for named type unless we introducing explicit recursive type aka. mu type.
    -}
    {- for mutual recursive type, we attach new generated metavariable to all unsolved type, and keep recording the constraint
    -- and unify the whole constraints at some point.
    -}
    -- go (TypConF mt mtn) = do
    --   (s1, term1 :@ k1) <- mt
    --   ((sn, kn), termn) <- mapM (\(s, t :@ k) -> ((s, [k]), t)) <$> sequence mtn
    --   k <- freshName
    --   return ((k1 :<> foldr (::>) k kn) : s1 <> sn, TypCon term1 termn `annotate` k)
    -- go (TypRepF mr) = return . pure $ TypRep r `annotate` KindType
