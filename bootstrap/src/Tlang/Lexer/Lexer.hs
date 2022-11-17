module Tlang.Lexer.Lexer
  ( lexer
  , integer, natural
  , float
  , parens, braces, brackets
  , stringLiteral
  , commaSep, semiSep
  , identifier, operator
  , reserved, reservedOp
  )
where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser u
lexer = Token.makeTokenParser language
  where language = emptyDef
                   { Token.commentLine = "//"
                   , Token.commentStart = "/*"
                   , Token.commentEnd = "*/"
                   , Token.nestedComments = True
                   , Token.caseSensitive = True
                   , Token.reservedNames = [ "fn", "let", "data", "if", "then"
                                           , "else", "match", "module", "export"
                                           , "unsafe", "ref"
                                           ]
                   , Token.reservedOpNames = [";", ":"]
                   -- , Token.opLetter = oneOf "!#$%&*+,-./:;<=>?@\\^_|~"
                   }

integer, natural :: Parsec String u Integer
integer = Token.integer lexer
natural = Token.natural lexer

float :: Parsec String u Double
float = Token.float lexer

parens, braces, brackets :: Parsec String u a -> Parsec String u a
parens = Token.parens lexer
braces = Token.braces lexer
brackets = Token.brackets lexer

stringLiteral :: Parsec String u String
stringLiteral = Token.stringLiteral lexer

commaSep, semiSep :: Parsec String u a -> Parsec String u [a]
commaSep = Token.commaSep lexer
semiSep = Token.semiSep lexer

identifier, operator :: Parsec String u String
identifier = Token.identifier lexer
operator = Token.operator lexer

reserved, reservedOp :: String -> Parsec String u ()
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
