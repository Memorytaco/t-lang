module Tlang.AST.Module
  ( Module (..)
  , ModulePath (..)
  , Declaration (..)
  , Import (..)
  , Use (..)
  , TypDecl
  , Op
  )
where

import Tlang.AST.Expr
import Tlang.AST.Type
import Tlang.AST.Operator

import Data.List (intercalate)

-- | default operator type
type Op = Operator String

-- | language module definition
data Module name sym syminfo op tname info =
  Module { mPath :: ModulePath name
         , mSyms :: [Import name sym syminfo]
         , mDecl :: [Declaration op tname info]
         } deriving (Show, Eq)

data ModulePath s = ModulePath [s] s deriving (Eq, Ord)

instance (Show s) => Show (ModulePath s) where
  show (ModulePath ls e) = intercalate "/" $ show <$> (ls <> [e])

instance {-# Incoherent #-} Show (ModulePath String) where
  show (ModulePath ls e) = intercalate "/" (ls <> [e])

-- | Import moduleName, symbols and qualified name
data Import name sym info = Import (ModulePath name) [Use sym info] (Maybe sym) deriving (Show, Eq)

-- | symbol in import list
-- Use representation of symbol and its definition
data Use sym info = Use sym info deriving (Show, Eq, Functor)

-- | toplevel type declaration, to assign names to types. aka. named type.
type TypDecl = NamedType

-- | toplevel definition in a module
data Declaration op tname info
  = LetD info (Expr op info)            -- ^ toplevel binding, used to declare value and function
  | TypD (TypDecl tname ())      -- ^ user defined data type
  | FixD op                             -- ^ fixity of user defined operator
  | FnD info (Maybe (Lambda op info))   -- ^ function marked with `fn` keywords, used to communicate between c and host lang
  deriving (Show, Eq)
