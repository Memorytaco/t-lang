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
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (void, forM, forM_)
import Control.Lens ((^.))

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

module'
  :: forall e m set
   . ( ShowErrorComponent e, MonadParsec e Text m, MonadFail m
     , HasState "OperatorStore" OperatorStore m
     , Item (UserOperator Text) :<: set)
  => [Module set Name] -> m (Decl set Name)
  -> m () -> m (Module set Name)
module' ms declaraton e = do
  whiteSpace
  name <- reserved "module" *> moduleName <* reservedOp ";;"
  uses <- many stmtUse >>= \deps -> do
    forM deps \(pos, u@(Use namespace@(Alias from _) imps)) -> region (setErrorOffset pos) do
      forM_ imps \(Alias sym _) -> do
        case isDeclStoreOf @(Item (UserOperator Text)) . (^. moduleDecls) <$> lookUpModule from ms of
          Just entries -> case filter (\(Item _ _name) -> _name == sym) entries of
                        [] -> fail $
                          "module (" <> show namespace <> ") does not contain definition for" <> show sym
                        ops -> forM_ ops putIntoEnv
          Nothing -> fail
            $ "module (" <> show namespace <> ") doesn't contain operator definition"
      return u
  decls <- many declaraton <* e
  return (Module name uses $ DeclStore decls)
  where
    lookUpModule :: ModuleName -> [Module set Name] -> Maybe (Module set Name)
    lookUpModule name = find $ (== name) . _moduleHeader
    putIntoEnv (Item (UserOperator op) _) = modify @"OperatorStore" (op:)
