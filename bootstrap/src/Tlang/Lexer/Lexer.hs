module Tlang.Lexer.Lexer
where

import Text.Parsec.String
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

-- Define language token parser

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser language
  where language = emptyDef
                   { Token.commentLine = "//"
                   , Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.nestedComments = True
                   , Token.caseSensitive = True
                   , Token.reservedNames = ["fn", "let", "data", "if", "then", "else", "match", "module", "export", "unsafe", "ref"]
                   , Token.reservedOpNames = [";", ":", "="]
                   }

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

commaSep :: Parser a -> Parser [a]
commaSep = Token.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer
