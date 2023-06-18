module Tlang.AST.Type
  ( Type (..)
  , TypeF (..)
  , TypeAssert (..)
  , Kind (..)
  , KindF (..)
  , (:==) (..)

  , Variance (..)

  , Bound (..)
  , Bounds (..)
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)
import Tlang.TH (fixQ)

-- | type representation. parameterised with some extensions.
-- please refer to `Tlang.Extension.Type` for all available options.
data Type rep prm bind inj name
  = TypPht                                      -- ^ empty type (bottom type) or "forall a. a" type
  | TypVar name                                 -- ^ refer to named type or type variable
  | TypRep (rep (Type rep prm bind inj name))  -- ^ type representation, the concrete one, will always be with kind '*'
                                                --   It has syntax level support
  | TypPrm (prm (Type rep prm bind inj name)) -- ^ type level Primitive prmtructor. natural number, string, record and so on.
                                                --   It also include sub structure like records and variants.
                                                --   Everything that could __literally__ written in source code.
  | TypCon (Type rep prm bind inj name)        -- ^ type application or type prmtructor
           [Type rep prm bind inj name]
  | TypLet (bind (Type rep prm bind inj name)) -- ^ a uniform way to represent arbitrary binding logic
           (Type rep prm bind inj name)
  | TypInj (inj (Type rep prm bind inj name))  -- ^ Allow artibrary injection, to provide further information of syntax tree
  deriving (Functor, Foldable, Traversable)

instance (Functor prm, Functor bind, Functor inj, Functor rep) => Applicative (Type rep prm bind inj) where
  pure = TypVar
  TypPht <*> _ = TypPht
  TypVar f <*> a = f <$> a
  TypRep ff <*> a = TypRep $ (<*> a) <$> ff
  TypPrm ff <*> a = TypPrm $ (<*> a) <$> ff
  TypCon a as <*> b = TypCon (a <*> b) ((<*> b) <$> as)
  TypLet ff a <*> b = TypLet ((<*> b) <$> ff) (a <*> b)
  TypInj ff <*> a = TypInj ((<*> a) <$> ff)

instance (Functor prm, Functor bind, Functor inj, Functor rep) => Monad (Type rep prm bind inj) where
  TypPht >>= _ = TypPht
  TypVar v >>= f = f v
  TypPrm fa >>= f = TypPrm ((>>= f) <$> fa)
  TypCon m ms >>= f = TypCon (m >>= f) ((>>= f) <$> ms)
  TypLet fa m >>= f = TypLet ((>>= f) <$> fa) (m >>= f)
  TypRep fa >>= f = TypRep ((>>= f) <$> fa)
  TypInj fa >>= f = TypInj ((>>= f) <$> fa)

deriving instance
  ( Eq (inj  (Type rep prm bind inj name))
  , Eq (bind (Type rep prm bind inj name))
  , Eq (prm (Type rep prm bind inj name))
  , Eq (rep (Type rep prm bind inj name))
  , Eq name
  ) => Eq (Type rep prm bind inj name)

deriving instance
  ( Ord (inj  (Type rep prm bind inj name))
  , Ord (bind (Type rep prm bind inj name))
  , Ord (prm (Type rep prm bind inj name))
  , Ord (rep (Type rep prm bind inj name))
  , Ord name
  ) => Ord (Type rep prm bind inj name)

instance
  ( Show name
  , Show (rep (Type rep prm bind inj name))
  , Show (inj (Type rep prm bind inj name))
  , Show (bind (Type rep prm bind inj name))
  , Show (prm (Type rep prm bind inj name))
  ) => Show (Type rep prm bind inj name) where
  show TypPht = "⊥"
  show (TypRep t) = show t
  show (TypPrm lit) = show lit
  show (TypVar name) = show name
  show (TypLet binder body) = "let { " <> show binder <> " = " <> show body <> " }"
  show (TypCon t ts) = "(" <> show t <> " " <> show ts <> ")"
  show (TypInj anno) = show anno

-- | type prmtraint
data TypeAssert msg typ
  = TACons typ typ [typ]  -- ^ actual worker, predicate
  | TAAnd (TypeAssert msg typ) [TypeAssert msg typ] -- ^ and connective
  | TATrue      -- ^ always Succeed
  | TAFail msg  -- ^ failed with a message, the message may be empty
  deriving (Show, Eq)

-- | MLF bounded quantifier
data Bound name typ
  = name :~ typ -- ^ equality relation
  | name :> typ -- ^ lower bound or subsume
  deriving (Show, Ord, Eq, Functor, Traversable, Foldable)
$(deriveBifunctor ''Bound)

-- | type Prefixs qual name typ = [Prefix qual name typ]
newtype Bounds name typ = Bounds [Bound name typ] deriving (Show, Ord, Eq, Functor, Traversable, Foldable)
$(deriveBifunctor ''Bounds)

-- | type variance
data Variance = InVar | CoVar | ContraVar deriving (Show, Eq, Ord)

-- | named type. assign name to type and allow
-- recursive type and type level lambda also.
-- no type level literal is supported now.
data name :== typ = name :== typ  -- ^ pick a name to encapsulate a type, and turn a unnamed type to named type.
                                  --   other information including variable and kind is in `Type` itself.
                                  --   definition for type level operator, which should be prefixed with ":"
  deriving (Show, Eq, Functor)

$(deriveBifunctor ''(:==))

-- | type kind representation, any kind, normal kind (*) and higher kind
-- TODO: refactor the structure
data Kind f name
  = KindType                      -- ^ concrete type, language's builtin type kind.
  | KindRef name                  -- ^ type kinds other than KindRef and KindType,
                                  --   and represent any type level thing, literal, value, lifted prmtructor, etc.
  | KindAbs name (Kind f name)    -- ^ quantified kind variable, well, this is...
  | Kind f name ::> Kind f name   -- ^ something like "* -> *", means this is a type prmtructor or something else.
  | KindLift (f (Kind f name))    -- ^ extend functionality

deriving instance (Eq (f (Kind f name)), Eq name) => Eq (Kind f name)
deriving instance (Functor f) => Functor (Kind f)

instance (Show name, Show (f (Kind f name))) => Show (Kind f name) where
  show KindType = "*"
  show (KindRef name) = show name
  show (KindAbs v name) = show v <> ". " <> show name
  show (v@(_ ::> _) ::> a) = "(" <> show v <> ")" <> " -> " <> show a
  show (a ::> b) = show a <> " -> " <> show b
  show (KindLift anno) = show anno

infixr 5 ::>

makeBaseFunctor $ fixQ [d|
  instance (Functor rep, Functor inj, Functor bind, Functor prm) => Recursive (Type rep prm bind inj name)
  instance (Functor f) => Recursive (Kind f name)
  |]
deriving instance (Show name, Show (rep r), Show r, Show (prm r), Show (bind r), Show (inj r)) => Show (TypeF rep prm bind inj name r)
