module Tlang.AST.Type
  ( Type (..)
  , TypeF (..)
  , TypeLit (..)
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

import Data.List (intercalate)

-- | type representation
data Type label name lit bind c f rep
  = TypBot                                        -- ^ empty type or the bottom type
  | TypUni                                        -- ^ unit type, represented as **()**
  | TypRep rep                                    -- ^ type representation, the concrete one, will always be with kind '*'
  | TypLit lit                                    -- ^ type level literal, natural number, string and all that
  | TypRef name                                   -- ^ refer to named type or type variable
  | TypTup [Type label name lit bind c f rep]                   -- ^ builtin tuple
  | TypRec [(label, Type label name lit bind c f rep)]          -- ^ product type
  | TypSum [(label, Maybe (Type label name lit bind c f rep))]  -- ^ variant type, grouped label type
  | TypPie (c (Type label name lit bind c f rep))   -- ^ type constraint, a predicate
           (Type label name lit bind c f rep)
  | TypApp (Type label name lit bind c f rep)       -- ^ type application
           (Type label name lit bind c f rep)
           [Type label name lit bind c f rep]
  | TypEqu (bind (Type label name lit bind c f rep))  -- ^ equi-recursive type
           (Type label name lit bind c f rep)
  | TypAll (bind (Type label name lit bind c f rep))  -- ^ universal quantified type, using de bruijn indice. this needs help from `name` type
           (Type label name lit bind c f rep)
  | TypAbs (bind (Type label name lit bind c f rep))  -- ^ higher kinded type, naming an incompleted type
           (Type label name lit bind c f rep)
  | TypNest (f (Type label name lit bind c f rep))    -- ^ wrap kind information into type, annotation for type

-- | type level literal value
data TypeLit
  = TLNat Integer -- ^ natural num
  | TLStr String  -- ^ constant string
  | TLNum Double  -- ^ floating num
  deriving (Show, Eq)

-- | type constraint
data TypeAssert msg typ
  = TACons typ typ [typ]  -- ^ actual worker, predicate
  | TAAnd (TypeAssert msg typ) [TypeAssert msg typ] -- ^ and connective
  | TATrue      -- ^ always Succeed
  | TAFail msg  -- ^ failed with a message, the message may be empty
  deriving (Show, Eq)

deriving instance (Functor f, Functor c, Functor bind) => Functor (Type label name lit bind c f)
deriving instance
  ( Eq (f (Type label name lit bind c f rep))
  , Eq (c (Type label name lit bind c f rep))
  , Eq (bind (Type label name lit bind c f rep))
  , Eq label, Eq lit, Eq name, Eq rep
  ) => Eq (Type label name lit bind c f rep)

instance
  ( Show label, Show name, Show rep, Show lit
  , Show (bind (Type label name lit bind c f rep))
  , Show (f (Type label name lit bind c f rep))
  , Show (c (Type label name lit bind c f rep))
  ) => Show (Type label name lit bind c f rep) where
  show TypBot = "⊥"
  show TypUni = "()"
  show (TypRep t) = show t
  show (TypLit lit) = show lit
  show (TypRef name) = show name
  show (TypTup ts) = "(" <> intercalate ", " (show <$> ts) <> ")"
  show (TypRec ts) =
    let mapper (a, b) = show a <> " = " <> show b
     in "{" <> intercalate ", " (mapper <$> ts) <> "}"
  show (TypSum ts) =
    let mapper (a, b) = show a <> maybe "" ((" = " <>) . show) b
     in "<" <> intercalate ", " (mapper <$> ts) <> ">"
  show (TypEqu bind t) = "μ" <> show bind <> ". " <> show t
  show (TypAll bind t) = "∀" <> show bind <> ". " <> show t
  show (TypAbs bind t) = "\\" <> show bind <> ". " <> show t
  show (TypPie constraint t) = show constraint <> " => " <> show t
  show (TypApp t1 t2 tn) = "(" <> show t1 <> " " <> show t2 <> " " <> show tn <> ")"
  show (TypNest anno) = show anno

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

type FoldFunctor f = (Functor f, Traversable f, Foldable f)
makeBaseFunctor [d| instance (FoldFunctor c, FoldFunctor f, FoldFunctor bind) => Recursive (Type label name lit bind c f rep) |]
makeBaseFunctor [d| instance (FoldFunctor f) => Recursive (Kind f name) |]
