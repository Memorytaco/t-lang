module Tlang.AST.Module
  ( Module (..)
  , ModuleID (..)
  , ModuleSource
  , Declaration (..)
  , FnSymbol (..)
  , Use (..)
  )
where

import Tlang.AST.Expr
import Tlang.AST.Type
import Tlang.AST.Operator
import Tlang.AST.Decl

import Data.List (intercalate)

-- | language module definition
data Module typ name info
  = Module ModuleSource ModuleID [Use info] [Declaration typ name]
  deriving (Show, Eq, Functor)

type ModuleSource = String

data ModuleID = ModuleID [String] String deriving (Eq, Ord)
instance Show ModuleID where
  show (ModuleID prefix name) = intercalate "/" $ prefix <> [name]

-- | toplevel definition in a module
data Declaration typ name
  = FixD (Operator String)  -- ^ fixity of user defined operator
  | TypD (name :== typ)     -- ^ user defined data type
  | TypF (name :== typ)     -- ^ type alias, aka, type level function
  | LetD name (Expr typ ((:@) typ) name)  -- ^ toplevel binding, used to declare value and function
  | FnD name typ (FnSymbol typ name)      -- ^ function marked with `fn` keywords, used to communicate between c and host lang
  deriving (Show, Eq)

data FnSymbol typ name
  = FnDefault       -- ^ default setting using declared name
  | FnSymbol String -- ^ external symbol name
  | FnDecl (Lambda typ ((:@) typ) name) -- ^ export a function out
  deriving (Show, Eq)

-- | module path fragment
newtype Frag
  = Frag String
  deriving (Show, Eq)

-- | a `ModuleName` is composed by multiple `Frag`.
-- Language takes a style of unix file path. Everything fits in
-- unix file path is supported by `ModuleName` though it is limited
-- in some language context.
data ModuleName = ModuleName [Frag] Frag deriving (Show, Eq)

data Mod decls info
  = Mod
    { mmName :: ModuleName        -- ^ Module name
    , mmUses :: [Use info]        -- ^ Module imports, including lexical items
    , mmDecl :: Decls decls info  -- ^ Module declarations
    , mmItem :: [Operator String] -- ^ Module lexical item, ignore its value or type binding.
    }

-- | A use statement to import symbol name.
-- Use (origin name, current name) [symbol list]
data Use info = Use (ModuleID, ModuleID) [info] deriving (Show, Eq, Functor)

