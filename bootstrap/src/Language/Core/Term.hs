{-# LANGUAGE QuantifiedConstraints #-}
{- | Highly experimental core definition for combining Free, Cofree and Bound (nested type as debruijn index)

  see https://mail.haskell.org/pipermail/haskell-cafe/2015-July/120579.html
  and https://www.reddit.com/r/haskell/comments/47t5r3/ast_typing/
  and https://www.reddit.com/r/haskell/comments/1f91w3/the_ast_typing_problem/
  and http://blog.ezyang.com/2013/05/the-ast-typing-problem/
  and https://hackage.haskell.org/package/Annotations for a recommended package

  The original problem comes from that we can't have nested type and Cofree annotation for every AST node
  at the same time. Some high level type wrapper tricks are needed to resolve this one.
  -}
module Language.Core.Term

where

-- | This is a `Free` monad
data Term f a
  = Var a
  | Term (f (Term f a))
  deriving (Functor, Foldable, Traversable)

instance Functor f => Applicative (Term f) where
  pure = Var
  Var f <*> Var a = Var (f a)
  f <*> Term fa = Term ((f <*>) <$> fa)
  Term ff <*> a = Term ((<*> a) <$> ff)
instance Functor f => Monad (Term f) where
  Var a >>= f = f a
  Term fma >>= f = Term ((>>= f) <$> fma)

-- | original sample to explain the idea
-- with `Compose` in `Data.Functor.Compose`

-- newtype Expr1 f a = Expr1
--   (Term (Base (Compose f (Expr1 f))) a)
--   deriving Functor via Term (Base (Compose f (Expr1 f)))

-- data Base f e = App (f e) (f e) | Lam (Bound () f e)
--   deriving (Functor, Foldable, Traversable)


-- | one possible definition for expression
newtype Expr1 x f a = Expr1 (Term (x (f (Expr1 x f))) a)
deriving instance (forall g. Functor (x g)) => Functor (Expr1 x f)

newtype Plain f e = Plain (f e)

data (f :++: g) h a
  = Inl1 (f h a)
  | Inr1 (g h a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | an extension to express application concept
data App f e = App (f e) (f e)
  deriving (Functor, Foldable, Traversable)

-- | a different style of application
data App2 f e = App2 (f e) [f e]
  deriving (Functor, Foldable, Traversable)

-- | see Scope in https://hackage.haskell.org/package/bound
newtype Bound name f a = Bound (f (Either name (f a)))
  deriving (Functor, Foldable, Traversable)

