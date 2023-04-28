module Tlang.Parser.Module
  ( 
  
    module'
  , ParseModuleType
  , moduleHeader
  , parseModule

  )
where

import Text.Megaparsec

import Tlang.Parser.Lexer

import Tlang.AST
import Tlang.AST.Class.Decl
import Tlang.Generic ((:<:))
import Tlang.Extension.Decl

import Data.Text (Text)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Control.Monad (void, forM, forM_)
import Control.Applicative (liftA2)
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Monad.Trans (lift)

type ParseModuleType decls = Module decls Symbol

moduleName :: ShowErrorComponent e => ParsecT e Text m ModuleName
moduleName = liftA2 ModuleName init last <$> (Frag <$> identifier) `sepBy1` (string "/")

-- | parsing module header
-- a header is composed by
-- 1. a module name
-- 2. import statements
moduleHeader :: (ShowErrorComponent e)
             => ParsecT e Text m ([Use Symbol], ModuleName)
moduleHeader = do
  whiteSpace
  name <- reserved "module" *> moduleName <* reservedOp ";;"
  deps <- many stmtUse
  whiteSpace
  return (snd <$> deps, name)

item :: ShowErrorComponent e => ParsecT e Text m Symbol
item = Symbol <$> identifier <|> Op <$> operator

stmtUse :: ShowErrorComponent e => ParsecT e Text m (Int, Use Symbol)
stmtUse = do
  pos <- stateOffset <$> getParserState
  void $ reserved "use"
  name <- NameAlias <$> moduleName <*> (optional (reserved "as" *> moduleName) <?> "qualified name")
  items <- optional . parens $ (flip NameAlias Nothing <$> item) `sepBy` symbol ","
  void $ reservedOp ";;"
  return (pos, Use name $ fromMaybe [] items)

infixl 4 ??
(??) :: forall decl decls info. (decl :<: decls, Query decl)
     => Module decls info -> (info -> Bool) -> [decl info]
_mod ?? info = queryAll info (mmDecl _mod)

module' :: (ShowErrorComponent e, Monad m, UserItem :<: decls)
        => [Module decls Symbol]
        -> ParsecT e Text (StateT ([Operator String], [Operator String]) m) (Decl decls Symbol)
        -> ParsecT e Text (StateT ([Operator String], [Operator String]) m) (Module decls Symbol, [Module decls Symbol])
module' ms declaraton = do
  whiteSpace
  name <- reserved "module" *> moduleName <* reservedOp ";;"
  uses <- many stmtUse >>= \deps -> do
    forM deps \(pos, u@(Use namespace@(NameAlias from _) imps)) -> region (setErrorOffset pos) do
      forM_ imps \(NameAlias sym _) -> do
        case lookUpModule from ms >>= pure . (`itemOf` (== sym)) of
          Just ops -> lift (forM_ ops putIntoEnv)
          Nothing -> fail
            $ "module (" <> show namespace <> ") does not contain definition for " <> show sym <> " or it does not exist"
      return u
  decls <- many declaraton <* eof
  return (Module name uses $ Decls decls, ms)
  where
    itemOf = (??) @UserItem
    lookUpModule :: ModuleName -> [Module decls Symbol] -> Maybe (Module decls Symbol)
    lookUpModule name = find $ (== name) . mmName
    putIntoEnv :: (Monad m) => UserItem a -> StateT ([Operator String], [Operator String]) m ()
    putIntoEnv (UserItem _ ops _) = forM_ ops \op@(Operator _ _ _ s) -> do
      modify \(tys, trs) -> if head s == ':' then (op:tys, trs) else (tys, op:trs)

-- | parse a new module basing on existed module
parseModule
  :: (ShowErrorComponent e, Monad m, UserItem :<: decls)
  => String -> ([Operator String], [Operator String]) -> [Module decls Symbol]
  -> ParsecT e Text (StateT ([Operator String], [Operator String]) m) (Decl decls Symbol)
  -> Text
  -> m (Either (ParseErrorBundle Text e) (Module decls Symbol, [Module decls Symbol]), ([Operator String], [Operator String]))
parseModule source op ms decl txt = flip runStateT op $ runParserT (module' ms decl) source txt

-- playModule :: ([Operator String], [Operator String]) -> [Module decls Symbol] -> Text -> IO ()
-- playModule op ms txt =
--   let m = runModule @Void "stdin" op ms txt <&> fst
--    in m >>= putStrLn . either errorBundlePretty show
