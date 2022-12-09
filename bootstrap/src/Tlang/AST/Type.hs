module Tlang.AST.Type
  ( TypAnno (..)
  , Type (..)
  , TypeF (..)

  , TypName (..)
  , KindedName (..)
  , NamedType (..)
  , TypeKind (..)
  , KindVal (..)
  , (/->)
  )
where

import Data.Functor.Foldable.TH

import Data.List (intercalate)

-- | type representation
data Type name rep
  = TypBottom                               -- ^ empty type or the bottom type
  | TypUnit                                 -- ^ unit type, represented as **()**
  | TypRep rep                              -- ^ type representation, the concrete one, will always be with kind '*'
  | TypRef name                             -- ^ refer to named type or type variable
  | TypAll name (Type name rep)             -- ^ universal quantified type, using de bruijn indice. this needs help from `name` type
  | TypAbs name (Type name rep)             -- ^ higher kinded type, naming an incompleted type
  | TypTup [Type name rep]                  -- ^ builtin tuple
  | TypRec [(name, (Type name rep))]        -- ^ product type, can be used to encode both tuple and record
  | TypSum [(name, Maybe (Type name rep))]  -- ^ variant type
  | TypApp (Type name rep) (Type name rep)  -- ^ type application
  deriving (Eq)

-- | named type. assign name to type and allow
-- recursive type and type level lambda.
data NamedType name t
  = TypAs name (Type name t)    -- ^ pick a name to encapsulate a type, and turn a unnamed type to named type.
                                --   other information including variable and kind is in `Type`
  | TypOpAs name (Type name t)  -- ^ definition for type level operator, which should be prefixed with ":"
  deriving (Show, Eq)

-- | Unresolved type name representation, used in parser only.
data TypName op = TypLabel String | TypName String | TypNameOp op deriving (Eq)

-- | be isomorphic with `TypName`, augmented with kind information.
-- `TypLabel` is transformed into a term level constructor, which is both a type constructor and term constructor
--    depending on the type itself. Note, this kind of constructor may not construct a concrete type.
data KindedName k s = TermCons k s | TypSym k Bool s | TypBound k s Integer deriving (Show, Eq)

-- | type kind representation, any kind, normal kind (*) and higher kind
data TypeKind k = KindAny                             -- ^ represent any type level thing, literal, value, lifted constructor, etc.
                | KindType                            -- ^ concrete type, language's builtin type kind.
                | KindCons k                          -- ^ type kinds other than KindCons and KindType
                | KindCat (TypeKind k) (TypeKind k)   -- ^ something like "* -> *", means this is a type constructor or something else.
                deriving (Eq)

infixr 8 /->
(/->) = KindCat

-- | kind symbol used in `TypeKind`, it can hold any thing.
newtype KindVal = KindVal String deriving (Eq, Ord)
instance (Show KindVal) where
  show (KindVal s) = s

instance Show k => Show (TypeKind k) where
  show (KindAny) = "⊤"
  show (KindType) = "*"
  show (KindCons k) = show k
  show (KindCat a b) = show a <> " -> " <> show b

instance (Show name, Show rep) => Show (Type name rep) where
  show TypBottom = "⊥"
  show TypUnit = "()"
  show (TypRep t) = show t
  show (TypRef name) = show name
  show (TypAll name typ) = show name <> ". " <> show typ
  show (TypAbs t1 t2) = "\\" <> show t1 <> ". " <> show t2
  show (TypTup ts) = "(" <> intercalate ", " (show <$> ts) <> ")"
  show (TypRec ts) =
    let mapper (a, b) = show a <> " = " <> show b
     in "{" <> intercalate ", " (mapper <$> ts) <> "}"
  show (TypSum ts) =
    let mapper (a, b) = show a <> maybe "" ((" = " <>) . show) b
     in "<" <> intercalate ", " (mapper <$> ts) <> ">"
  show (TypApp t1 t2) = "(" <> show t1 <> " " <> show t2 <> ")"

instance Show op => Show (TypName op) where
  show (TypName s) = s
  show (TypLabel s) = "@" <> s
  show (TypNameOp op) = show op

-- | type annotation with full power of the type system
data TypAnno op
  = TypInfer                        -- ^ to let compiler fill in the type information
  | TypAnno (Type (TypName op) ())  -- ^ user annotated type information
  deriving (Show, Eq)

$(makeBaseFunctor ''Type)
