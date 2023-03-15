module Tlang.Parser.Module
  ( ParsedModule
  , ParseModuleType

  , parseModule
  , playModule
  , runModule
  , buildGraph
  )
where

import Text.Megaparsec

import Tlang.Parser.Lexer
import Tlang.Parser.Type (ParseType)
import Tlang.Parser.Decl (ParseDeclType, declaration)

import Tlang.AST

import Data.Functor (($>), (<&>))
import Data.Text (Text)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Control.Monad (void, forM)
import Control.Monad.Identity (Identity)
import Control.Applicative (liftA2)
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Monad.Trans (lift)

type ParseModuleType c f = Module (ParseType c f) Symbol (Symbol, Declaration (ParseType c f) Symbol)
type ParsedModule = ParseModuleType None Identity

moduleID :: ShowErrorComponent e => ParsecT e Text m ModuleID
moduleID = liftA2 ModuleID init last <$> identifier `sepBy1` symbol "/"

item :: ShowErrorComponent e => ParsecT e Text m Symbol
item = Symbol <$> identifier <|> Op <$> operator

use :: ShowErrorComponent e => ParsecT e Text m (Int, Use Symbol)
use = do
  pos <- stateOffset <$> getParserState
  void $ reserved "use"
  name <- moduleID
  qualified <- optional (reserved "as" *> (ModuleID [] <$> identifier)) <?> "qualified name"
  let prefix = maybe (name, name) (name,) qualified
  items <- optional . parens $ item `sepBy` symbol ","
  void $ reservedOp ";;"
  return (pos, Use prefix $ fromMaybe [] items)

parseModule :: (ShowErrorComponent e, Monad m)
            => [ParsedModule] -> ModuleSource -> ParsecT e Text (StateT ([Operator String], [Operator String]) m) (ParsedModule, [ParsedModule])
parseModule ms source = do
  whiteSpace
  path <- reserved "module" *> moduleID <* reservedOp ";;"
  deps <- many use >>= \deps -> do
    forM deps \(pos, Use prefix@(origin, quali) imps) -> region (setErrorOffset pos) do
      syms <- forM imps \sym -> do
        let msg = "module " <> show quali <> " doesn't contain definition for " <> show sym
                <> " or module " <> show origin <> " doesn't exist"
        dec <- maybe (fail msg) return $ lookUpModule origin ms >>= lookUpName sym
        lift (putIntoEnv dec) $> (sym, dec)
      return $ Use prefix syms
  decls <- many declaration
  eof
  return (Module source path deps decls, ms)
  where
    lookUpName :: Symbol -> ParsedModule -> Maybe (ParseDeclType None Identity)
    lookUpName name (Module _ _ deps decs) =
      let predicate (LetD s _) = s == name
          predicate (TypD (s :== _)) = s == name
          predicate (FixD (Operator _ _ _ s)) = Op s == name
          predicate (FnD s _ _) = s == name
       in find predicate decs <|> foldl (<|>) Nothing (fmap (\(Use _ ls) -> lookup name ls) deps)
    lookUpModule :: ModuleID -> [ParsedModule] -> Maybe ParsedModule
    lookUpModule path = find \(Module _ mid _ _) -> mid == path
    putIntoEnv :: (Monad m) => ParseDeclType None Identity -> StateT ([Operator String], [Operator String]) m ()
    putIntoEnv (FixD op@(Operator _ _ _ s)) = modify \(tys, trs) ->
      if head s == ':' then (op:tys, trs) else (tys, op:trs)
    putIntoEnv _ = pure ()

-- | build up module dependency graph
buildGraph :: (ShowErrorComponent e)
           => ParsecT e Text m ([Use Symbol], ModuleID)
buildGraph = do
  whiteSpace
  path <- reserved "module" *> moduleID <* reservedOp ";;"
  deps <- many use
  whiteSpace
  return (snd <$> deps, path)

-- | parse a new module basing on existed module
runModule :: (ShowErrorComponent e, Monad m)
          => ModuleSource -> ([Operator String], [Operator String]) -> [ParsedModule] -> Text
          -> m (Either (ParseErrorBundle Text e) (ParsedModule, [ParsedModule]), ([Operator String], [Operator String]))
runModule source op ms txt = flip runStateT op $ runParserT (parseModule ms source) source txt

playModule :: ([Operator String], [Operator String]) -> [ParsedModule] -> Text -> IO ()
playModule op ms txt =
  let m = runModule @Void "stdin" op ms txt <&> fst
   in m >>= putStrLn . either errorBundlePretty show
