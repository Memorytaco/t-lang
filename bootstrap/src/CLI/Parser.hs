module CLI.Parser
  ( CommandLineOption (..)
  , command
  )
where

import Text.Parsec
import Text.Parsec.String

data CommandLineOption =
    REPL
  | HELP (Maybe String)
  | COMPILE String (Maybe String)
  deriving (Show, Eq)

help :: Parser CommandLineOption
help = do
  _ <- string "help" <|> try (string "--help") <|> string "-h" <?> "Help token"
  spaces
  topic <- optionMaybe (try $ many1 anyChar)
  return $ HELP topic

repl :: Parser CommandLineOption
repl = do
  _ <- string "repl" <?> "Repl token"
  return REPL

identifier :: Parser String
identifier = do
  let symbol = alphaNum <|> char '.' <|> char '_' <|> char '-'
  many1 symbol

compile :: Parser CommandLineOption
compile = do
  string "compile" >> spaces
  outs <- sepBy1 identifier spaces <?> "file list"
  return $ case outs of
    [a] -> COMPILE a Nothing
    [a, b] -> COMPILE a (Just b)
    (a:b:_) -> COMPILE a (Just b)

command :: [String] -> Either ParseError CommandLineOption
command ags = parse (try help <|> try repl <|> (compile <?> "Compile token")) "commandline" $ foldl1 (\a b -> a <> " " <> b) ags
