module Language.Parser.Module
  (
  -- ** module relevant parser
    module'
  , modulePrologue
  )
where

import Text.Megaparsec

import Language.Parser.Lexer

import Language.Core
import Language.Generic ((:<:))
import Language.Core.Extension.Decl

import Data.Text (Text)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (void, forM, forM_)

import Capability.State (HasState, modify)

moduleName :: (ShowErrorComponent e, MonadParsec e Text m, MonadFail m) => m ModuleName
moduleName = liftA2 ModuleName init last <$> (Name <$> identifier) `sepBy1` string "/"

-- | parsing module header
--
-- a header is composed by
--
-- 1. a module name
--
-- 2. use statements
modulePrologue :: (ShowErrorComponent e, MonadParsec e Text m, MonadFail m)
             => m ([Use Name], ModuleName)
modulePrologue = do
  whiteSpace
  name <- reserved "module" *> moduleName <* reservedOp ";;"
  deps <- many stmtUse
  whiteSpace
  return (snd <$> deps, name)

item :: (ShowErrorComponent e, MonadParsec e Text m, MonadFail m) => m Name
item = fmap Name $ identifier <|> operator

stmtUse :: (ShowErrorComponent e, MonadParsec e Text m, MonadFail m) => m (Int, Use Name)
stmtUse = do
  pos <- stateOffset <$> getParserState
  void $ reserved "use"
  name <- Alias <$> moduleName <*> (optional (reserved "as" *> moduleName) <?> "qualified name")
  items <- optional . parens $ (flip Alias Nothing <$> item) `sepBy` symbol ","
  void $ reservedOp ";;"
  return (pos, Use name $ fromMaybe [] items)

infixl 4 ??
(??) :: forall decl decls info. (decl :<: decls, Query decl)
     => Module decls info -> (info -> Bool) -> [decl info]
_mod ?? info = queryAll info (_moduleDecls _mod)

module'
  :: forall e m decls
   . ( ShowErrorComponent e, MonadParsec e Text m, MonadFail m
     , HasState "OperatorStore" OperatorStore m
     , Item (UserOperator Text) :<: decls)
  => [Module decls Name] -> m (Decl decls Name)
  -> m () -> m (Module decls Name)
module' ms declaraton e = do
  whiteSpace
  name <- reserved "module" *> moduleName <* reservedOp ";;"
  uses <- many stmtUse >>= \deps -> do
    forM deps \(pos, u@(Use namespace@(Alias from _) imps)) -> region (setErrorOffset pos) do
      forM_ imps \(Alias sym _) -> do
        case lookUpModule from ms <&> (`itemOf` (== sym)) of
          Just ops -> forM_ ops putIntoEnv
          Nothing -> fail
            $ "module (" <> show namespace <> ") does not contain definition for " <> show sym <> " or it does not exist"
      return u
  decls <- many declaraton <* e
  return (Module name uses $ Decls decls)
  where
    itemOf = (??) @(Item (UserOperator Text)) @decls
    lookUpModule :: ModuleName -> [Module decls Name] -> Maybe (Module decls Name)
    lookUpModule name = find $ (== name) . _moduleHeader
    putIntoEnv (Item (UserOperator op) _) = modify @"OperatorStore" (op:)
