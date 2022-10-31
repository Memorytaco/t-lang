module Tlang.Lexer.Comment
where

-- MODULE OF NO USE

import Text.Parsec.String

data Comment = CommentLine String | CommentBlock String deriving (Show, Eq)

data CommentDef = CommentLineDef String | CommentBlockDef String String deriving (Show, Eq)

line :: CommentDef
line = CommentLineDef "//"

block :: CommentDef
block = CommentBlockDef "/*" "*/"
