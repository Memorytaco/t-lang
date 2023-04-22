module Tlang.AST.Type
  ( Type (..)
  , TypeF (..)
  , TypeAssert (..)
  , Kind (..)
  , KindF (..)
  , (:==) (..)

  , Variance (..)

  , Bound (..)
  , Bounds
  )
where

import Data.Functor.Foldable.TH
import Data.Functor.Foldable (Recursive)
import Data.Bifunctor.TH (deriveBifunctor)

-- | type representation. parameterised with some extensions.
-- please refer to `Tlang.Extension.Type` for all available options.
data Type name cons bind inj rep
  = TypBot                                      -- ^ empty type or the bottom type
  | TypRep rep                                  -- ^ type representation, the concrete one, will always be with kind '*'
                                                --   It has syntax level support
  | TypRef name                                 -- ^ refer to named type or type variable
  | TypLit (cons (Type name cons bind inj rep)) -- ^ type level literal constructor. natural number, string and all that.
                                                --   It also include sub structure like records and variants.
                                                --   Everything that could __literally__ written in source code.
  | TypCon (Type name cons bind inj rep)        -- ^ type application or type constructor
           [Type name cons bind inj rep]
  | TypLet (bind (Type name cons bind inj rep)) -- ^ a uniform way to represent arbitrary binding logic
           (Type name cons bind inj rep)
  | TypInj (inj (Type name cons bind inj rep))  -- ^ Allow artibrary injection, to provide further information of syntax tree
  deriving (Functor, Foldable, Traversable)

deriving instance
  ( Eq (inj  (Type name cons bind inj rep))
  , Eq (bind (Type name cons bind inj rep))
  , Eq (cons (Type name cons bind inj rep))
  , Eq name, Eq rep
  ) => Eq (Type name cons bind inj rep)

instance
  ( Show name, Show rep
  , Show (inj (Type name cons bind inj rep))
  , Show (bind (Type name cons bind inj rep))
  , Show (cons (Type name cons bind inj rep))
  ) => Show (Type name cons bind inj rep) where
  show TypBot = "⊥"
  show (TypRep t) = show t
  show (TypLit lit) = show lit
  show (TypRef name) = show name
  show (TypLet binder body) = "let { " <> show binder <> " = " <> show body <> " }"
  show (TypCon t ts) = "(" <> show t <> " " <> show ts <> ")"
  show (TypInj anno) = show anno

-- | type constraint
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
  deriving (Show, Eq, Functor, Traversable, Foldable)
$(deriveBifunctor ''Bound)

-- | type Prefixs qual name typ = [Prefix qual name typ]
type Bounds name typ = [Bound name typ]

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
                                  --   and represent any type level thing, literal, value, lifted constructor, etc.
  | KindAbs name (Kind f name)    -- ^ quantified kind variable, well, this is...
  | Kind f name ::> Kind f name   -- ^ something like "* -> *", means this is a type constructor or something else.
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

makeBaseFunctor [d| instance (Traversable inj, Traversable bind, Traversable cons) => Recursive (Type name cons bind inj rep) |]
makeBaseFunctor [d| instance (Traversable f) => Recursive (Kind f name) |]
