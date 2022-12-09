module Tlang.Parser.Module
  ( ParsedModule

  , parseModule
  , playModule
  , runModule
  , buildGraph
  )
where

import Text.Megaparsec

import Tlang.Parser.Lexer
import Tlang.Parser.Declaration (declaration)

import Tlang.AST

import Data.Text (Text)
import Data.List (find)
import Data.Void (Void)
import Control.Monad (void, forM)
import Control.Applicative (liftA2)
import Control.Monad.Trans.State (StateT, runStateT, modify)
import Control.Monad.Trans (lift)

type ParsedModule = Module String String (Declaration Op (TypName Op) (UnTypedName Op)) Op (TypName Op) (UnTypedName Op)

modulePath :: ShowErrorComponent e => ParsecT e Text m (ModulePath String)
modulePath = liftA2 ModulePath init last <$> identifier `sepBy1` symbol "/"

item :: ShowErrorComponent e => ParsecT e Text m (Use String ())
item = identifier <|> operator >>= return . flip Use ()

use :: ShowErrorComponent e => ParsecT e Text m (Int, Import String String ())
use = do
  pos <- stateOffset <$> getParserState
  void $ reserved "use"
  name <- modulePath
  qualified <- optional (reserved "as" *> identifier) <?> "qualified name"
  syms <- optional . parens $ item `sepBy` symbol ","
  void $ reservedOp ";;"
  return (pos, Import name (maybe [] id syms) qualified)

parseModule :: (ShowErrorComponent e, Monad m)
            => [ParsedModule] -> ParsecT e Text (StateT ([Op], [Op]) m) (ParsedModule, [ParsedModule])
parseModule ms = do
  whiteSpace
  path <- reserved "module" *> modulePath <* reservedOp ";;"
  deps <- many use >>= \deps -> do
    forM deps \(pos, Import ps imps quali) -> region (setErrorOffset pos) do
      syms <- forM imps \(Use sym ()) -> do
        let msg = "module " <> show ps <> " doesn't contain definition for " <> sym
                <> " or module " <> show ps <> " doesn't exist"
        dec <- maybe (fail msg) return $ lookUpModule ps ms >>= lookUpName sym
        lift (putIntoEnv dec) *> return (Use sym dec)
      return $ Import ps syms quali
  decls <- many declaration
  eof
  return (Module path deps decls, ms)
  where
    lookUpName :: String -> ParsedModule -> Maybe (Declaration Op (TypName Op) (UnTypedName Op))
    lookUpName name m =
      let predicate (LetD (UnTypedName _ s) _) = s == name
          predicate (TypD (TypAs (TypName s) _)) = s == name
          predicate (TypD (TypOpAs (TypName s) _)) = s == name
          predicate (FixD (Operator _ _ _ s)) = s == name
          predicate (FnD (UnTypedName _ s) _) = s == name
          predicate _ = False
       in find predicate (mDecl m) <|> lookup name (fmap (\(Use s i) -> (s, i)) $ foldl (\ls (Import _ us _) -> us <> ls) [] (mSyms m))
    lookUpModule :: ModulePath String -> [ParsedModule] -> Maybe ParsedModule
    lookUpModule path = lookup path . fmap (\m -> (mPath m, m))
    putIntoEnv :: (Monad m) => Declaration Op (TypName Op) info -> StateT ([Op], [Op]) m ()
    putIntoEnv (FixD op@(Operator _ _ _ s)) = modify \(tys, trs) ->
      if head s == ':' then (op:tys, trs) else (tys, op:trs)
    putIntoEnv _ = pure ()

-- | build up module dependency graph
buildGraph :: (ShowErrorComponent e)
           => ParsecT e Text m ([Import String String ()], ModulePath String)
buildGraph = do
  whiteSpace
  path <- reserved "module" *> modulePath <* reservedOp ";;"
  deps <- many use
  whiteSpace
  return (snd <$> deps, path)

-- | parse a new module basing on existed module
runModule :: (ShowErrorComponent e, Monad m)
          => String -> ([Op], [Op]) -> [ParsedModule] -> Text
          -> m (Either (ParseErrorBundle Text e) (ParsedModule, [ParsedModule]), ([Op], [Op]))
runModule source op ms txt = flip runStateT op $ runParserT (parseModule ms) source txt

playModule :: ([Op], [Op]) -> [ParsedModule] -> Text -> IO ()
playModule op ms txt =
  let m = runModule @Void "stdin" op ms txt >>= return . fst
   in either errorBundlePretty show <$> m >>= putStrLn
