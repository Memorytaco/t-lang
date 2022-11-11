module Tlang.Type.Polymorphism
  ( variables

  , Symbol (..)
  , SymbolF (..)
  , UnifyContext
  , UnifyContextT (..)
  , GammaContext
  , ConstraintContext

  , addBinding
  , addConstraint
  , findBindingBy
  , substitute
  , domain
  , range
  , unify
  , freeVars
  , sigma
  , verify
  , reduce

  , (:==) (..)
  , (:=) (..)
  , (:|->) (..)
  , (:=|) (..)
  , (:<) (..)

  , (:|-) (..)
  , (:|) (..)
  )
where

import Control.Monad.State.Class (MonadState (..), modify)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Identity
import Data.Foldable (find)
import Data.Functor.Foldable.TH
import Data.Functor.Foldable

import Data.List (union, nub, intersect)

-- Exrpess equality of type
data (a :== b) = a :== b deriving (Show, Eq)
-- Express type equation to be solved, it is `and` connected
data (a := b) = a := b deriving (Show, Eq, Functor)
-- Express type substitution, map `a` to `b`
data (a :|-> b) = a :|-> b deriving (Show, Eq, Functor)
-- Solution of type `a` could be in `b` or simply `b`
data (a :=| b) = a :=| b deriving (Show, Eq, Functor)
-- Express type 'a' is subtype of `b`
data (a :< b) = a :< b deriving (Show, Eq)

infix 1 :=

data (ctx :|- term) = ctx :|- term deriving (Show, Eq, Functor)
data (term :| constraint) = term :| constraint deriving (Show, Eq, Functor)

-- use sym to represent a type name or typ variable and also term variable name, and type is what we actually want.
-- We represent arrow type here so now we don't need to struggle with original structure for language type.
data Symbol sym typ = UnSolved sym
                    | Solved typ
                    | Symbol sym typ :-> Symbol sym typ
                    deriving (Eq)

instance (Show sym, Show typ) => Show (Symbol sym typ) where
  show (UnSolved sym) = "??" <> show sym
  show (Solved typ) = show typ
  show (r1 :-> r2) = case r1 of
                       (_ :-> _) -> "(" <> show r1 <> ")" <> " :-> " <> show r2
                       _ -> show r1 <> " :-> " <> show r2

infixr 8 :->
$(makeBaseFunctor ''Symbol)

instance Functor (Symbol sym) where
  fmap _ (UnSolved sym) = UnSolved sym
  fmap f (Solved typ) = Solved $ f typ
  fmap f (fa :-> fb) = fmap f fa :-> fmap f fb

substitute :: forall sym typ. Eq sym => (sym :|-> Symbol sym typ) -> Symbol sym typ -> Symbol sym typ
substitute (name :|-> res) = cata go
  where
    go :: SymbolF sym typ (Symbol sym typ) -> (Symbol sym typ)
    go (UnSolvedF n) = if n == name then res else UnSolved n
    go (SolvedF n) = Solved n
    go (a :->$ b) = a :-> b

domain :: forall sym typ. (sym :|-> Symbol sym typ) -> sym
domain (n :|-> _) = n
range :: forall sym typ. (sym :|-> Symbol sym typ) -> Symbol sym typ
range (_ :|-> n) = n

type UnifyContext sym typ = UnifyContextT sym typ Identity

type GammaContext sym typ = [sym := Symbol sym typ]
type ConstraintContext sym typ = [Symbol sym typ := Symbol sym typ]

freeVars :: forall sym typ. (Eq sym) => Symbol sym typ -> [sym]
freeVars = cata go
  where
    go :: Base (Symbol sym typ) [sym] -> [sym]
    go (UnSolvedF n) = [n]
    go (SolvedF _) = []
    go (as :->$ bs) = as `union` bs

-- build solution out of the unified constraints
sigma :: forall sym typ. (Eq sym) => [sym :|-> Symbol sym typ] -> Symbol sym typ -> Symbol sym typ
sigma = merge . fmap substitute
  where
    merge :: [Symbol sym typ -> Symbol sym typ] -> Symbol sym typ -> Symbol sym typ
    merge = cata \case
      (Cons f t) -> f . t
      Nil -> id

-- verify a solution with a set of constraints
verify :: forall sym typ ele. (ele ~ Symbol sym typ, Eq typ, Eq sym)
       => [sym :|-> ele] -> [ele := ele] -> Bool
verify (sigma -> sol) = cata go
  where
    go Nil = True
    go (Cons (a := b) rs) = sol a == sol b && rs

-- rebuild the constraints, and remove unrecursive type variable.
-- rules:
--   solutions == either (const []) id (unify constraints)
reduce :: forall e sym typ. (Eq sym, e ~ Symbol sym typ)
       => [sym :|-> e] -> [e := e] -> [e := e]
reduce solutions constraints =
  let σ = sigma solutions
      frees = cata (\case Nil -> []; (Cons a rs) -> a `union` rs)
            . fmap (\(s1 := s2) -> freeVars s1 `union` freeVars s2)
            . fmap (\(s1 := s2) -> σ s1 := σ s2)
      vars = frees constraints
   in fmap (\(name :|-> e) -> UnSolved name := e)
    . filter (\(name :|-> _) -> name `elem` vars)
    $ solutions

-- a naive implementation of unifying type constraint.
-- propertes what a solution would occupy:
-- solution : [name :|-> var] :: [sym :|-> Symbol sym typ]
-- 1. names on the left would never occur in free variables list of right symbol (unless recursive type)
-- 2. (σ left == σ right) should hold for every element of [ left := right ]
unify :: forall m sym typ. (Eq sym, Eq typ, Show typ, Show sym, MonadError String m)
      => [Symbol sym typ := Symbol sym typ] -> m [sym :|-> Symbol sym typ]
unify = fmap refresh . refold shrink expand
  where
    expand [] = Nil
    expand (((a :-> b) := (c :-> d)) :xs) = expand $ [a := c, b := d] ++ xs
    expand (x :xs) = Cons x xs

    shrink :: ListF (Symbol sym typ := Symbol sym typ) (m [sym :|-> Symbol sym typ]) -> m [sym :|-> Symbol sym typ]
    shrink Nil = return []
    shrink (Cons (m := n) ts) = ts >>= \ls -> do
      let update = flip cata (substitute <$> ls) \case
            (Cons f t) -> f . t
            Nil -> id
      match (update m := update n) ls
      where
        match v@(Solved a := Solved b) as =
          if a == b then return as else throwError $ show a <> " is not equal with " <> show b <> " in " <> show v
        match (UnSolved a := Solved b) as = return $ (a :|-> Solved b) : as
        match (Solved a := UnSolved b) as = return $ (b :|-> Solved a) : as
        match (UnSolved a := UnSolved b) as = return $ (a :|-> UnSolved b) : as
        match (UnSolved v := (a :-> b)) as = return $ (v :|-> (a :-> b)) : as
        match ((a :-> b) := UnSolved v) as = return $ (v :|-> (a :-> b)) : as
        match (Solved v := v2@(_ :-> _)) _ =
          throwError $ "Try to match scala type " <> show v <> " with an arrow" <> show v2
        match (v2@(_ :-> _) := Solved v) _ =
          throwError $ "Try to match scala type " <> show v <> " with an arrow" <> show v2
        match (v1@(_ :-> _) := v2@(_ :-> _)) _ =
          throwError $ "Internal Error with " <> show v1 <> " and " <> show v2

    refresh :: [sym :|-> Symbol sym typ] -> [sym :|-> Symbol sym typ]
    refresh = apo go
      where
        go :: forall base. (base ~ (sym :|-> Symbol sym typ))
           => [base] -> Base [base] (Either [base] [base])
        go [] = Nil
        go (v@(name :|-> sym) : as) =
          let vars = freeVars sym
              availables = nub $ flip fmap as \(n :|-> _) -> n
              shared = intersect vars availables
           in case shared == [] of
                True -> fmap Right . project . (v:) $ (fmap (substitute v) <$> as)
                False -> fmap Left . project . (as <>) $ [name :|-> effect as sym]
                  where
                    effect = sigma . filter ((`elem` shared) . domain)

newtype UnifyContextT sym typ m term = UnifyContextT
  { unWrapUnifyContextT :: (GammaContext sym typ, ConstraintContext sym typ)
                        -> m (GammaContext sym typ :|- (term :| ConstraintContext sym typ))
  }

instance Functor m => Functor (UnifyContextT sym typ m) where
  fmap f (UnifyContextT runContext) = UnifyContextT \s -> flip fmap (runContext s)
    \(a :|- (t :| b)) -> a :|- (f t :| b)

instance (Monad m, Applicative m) => Applicative (UnifyContextT sym typ m) where
  pure t = UnifyContextT \(a, b) -> pure (a :|- (t :| b))
  (<*>) (UnifyContextT mf) (UnifyContextT ma) = UnifyContextT \s -> do
    (sa :|- (f :| sb)) <- mf s
    (sa1 :|- (a :| sb1)) <- ma (sa, sb)
    return (sa1 :|- (f a :| sb1))

instance (Applicative m, Monad m) => Monad (UnifyContextT sym typ m) where
  return = pure
  (>>=) (UnifyContextT ma) fm = UnifyContextT \s -> do
    (sa1 :|- (a :| sb1)) <- ma s
    fm a `unWrapUnifyContextT` (sa1, sb1)

instance Monad m => MonadState ([sym := Symbol sym typ], [Symbol sym typ := Symbol sym typ]) (UnifyContextT sym typ m) where
  state f = UnifyContextT \s -> do
    let (a, (b1, b2)) = f s
    return (b1 :|- (a :| b2))

instance MonadTrans (UnifyContextT sym typ) where
  lift ima = UnifyContextT \(c1, c2) -> do
    a <- ima
    return (c1 :|- (a :| c2))

addBinding :: Monad m => (sym := Symbol sym typ) -> UnifyContextT sym typ m ()
addBinding entry = modify (\(ts, v) -> (entry: ts, v))
addConstraint :: Monad m => (Symbol sym typ := Symbol sym typ) -> UnifyContextT sym typ m ()
addConstraint entry = modify (\s -> (entry:) <$> s)
findBindingBy :: Monad m => (sym -> Bool) -> UnifyContextT sym typ m (Maybe (sym := Symbol sym typ))
findBindingBy predicate = do
  (binding, _) <- get
  return $ flip find binding \(sym := _) -> predicate sym

variables :: [String]
variables =
  let infinit n = "'" <> name <> num
        where list = ((:"") <$> ['a' .. 'z']) `zip` (replicate 26 . ("" :) $ show <$> ([1..] :: [Integer]))
              (ixs, ixt) = quotRem n 26
              (name, num) = fmap (last . take (ixs + 1)) . last $ take (ixt + 1) list
  in infinit <$> [0..]

