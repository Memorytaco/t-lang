module Language.Parser.Lexer
  ( whiteSpace
  , lexeme
  , integer
  , float
  , parens, braces, brackets, angles
  , stringLiteral
  , commaSep, semiSep
  , identifier, identifier', operator
  , reserved, reservedOp
  , symbol
  , string
  )
where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lex
import Data.Functor ((<&>))
import Data.Text (Text, pack)

type TextParser e m = (ShowErrorComponent e, MonadParsec e Text m)

reservedNames :: [Text]
reservedNames = ["let", "in", "data", "module", "use", "foreign"]

whiteSpace :: TextParser e m => m ()
whiteSpace = Lex.space space1 (Lex.skipLineComment "//") (Lex.skipBlockCommentNested "/*" "*/")

lexeme :: TextParser e m => m a -> m a
lexeme = Lex.lexeme whiteSpace

integer :: (TextParser e m, Num a) => m a
integer = lexeme Lex.decimal

symbol :: TextParser e m => Text -> m Text
symbol = Lex.symbol whiteSpace

reserved, reservedOp :: (TextParser e m) => Text -> m Text
reserved t = lexeme . try $ string t <* notFollowedBy alphaNumChar
reservedOp t = lexeme . try $ string t <* notFollowedBy (oneOf ("!#$%&*+-./:;<=>?@\\^|~" :: String))

operator :: (TextParser e m, MonadFail m) => m Text
operator = lexeme $ do
  let reservedOps = [";;", ":", ",", "\\", "|", "[", "]", "{", "}", "(", ")", "()", "[]", "{}", "@", "=>"]
  -- ops <- some (oneOf ("(){}[]!#$%&*+,-./:;<=>?@\\^_|~" :: [Char]))
  ops <- pack <$> some (symbolChar <|> punctuationChar)
  if ops `elem` reservedOps
     then fail $ "unexpected reserved operator " <> show ops
     else return ops

identifier' :: (TextParser e m, MonadFail m) => m Text
identifier' = do
  name <- do
    c <- char '_' <|> letterChar <?> "identifier prefix"
    cs <- many (char '_' <|> alphaNumChar) <?> "identifier suffix"
    return $ pack (c : cs)
  if name `notElem` reservedNames
     then return name
     else fail $ "unexpected reserved name " <> show name

identifier :: (TextParser e m, MonadFail m) => m Text
identifier = lexeme identifier'

float :: (TextParser e m, RealFloat a) => m a
float = lexeme Lex.float

parens, braces, angles, brackets :: (TextParser e m) => m a -> m a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (reservedOp "<") (reservedOp ">")
brackets  = between (symbol "[") (symbol "]")

stringLiteral :: TextParser e m => m Text
stringLiteral = lexeme $ char '"' *> manyTill Lex.charLiteral (char '"') <&> pack

commaSep, semiSep :: (TextParser e m) => m a -> m [a]
commaSep = flip sepBy (reservedOp ",")
semiSep = flip sepBy (reservedOp ";")
