module Example
  ( Exp (..)
  , type ($-) (..)
  )
where

import Data.Bifunctor.TH (deriveBifunctor)

data b $- a
  = Bind b
  | Free a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

$(deriveBifunctor ''($-))

data Exp name a
  = Var a
  | App (Exp name a) (Exp name a)
  | Lam (Exp name (name $- Exp name a))
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative (Exp name) where
  pure = Var
  Var f <*> a = f <$> a
  App f1 f2 <*> a = App (f1 <*> a) (f2 <*> a)
  Lam f <*> a = Lam $ pure lift <*> f
    where
      lift (Bind x) = Bind x
      lift (Free g) = Free (g <*> a)

instance Monad (Exp name) where
  Var a >>= f = f a
  App a b >>= f = App (a >>= f) (b >>= f)
  Lam v >>= f = Lam $ v >>= \case
    Bind a -> return $ Bind a
    Free x -> return . Free $ x >>= f

