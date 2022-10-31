module Tlang.Syntax
where

-- language syntax, using parsing expression grammar.
-- Ref to https://en.wikipedia.org/wiki/Parsing_expression_grammar for definition.

-- The syntax is not fully determined yet and will get changed in the future.

{-

Global <- Function | TypeDefinition | Constant

Constant <- "let" Identifier TypeAnnotation "=" Literal

TypeDefinition <- "data" Identifier "{" TypeDefItem+ "}"
TypeDefItem <- (TypeRecordFieldPair | TypeSumDef) ";"
TypeRecordFieldPair <- Identifier TypeAnnotation
TypeSumDef <- Identifier "=" Type

Function <- "fn" Identifier TypeAnnotation? (";" / LambdaBlock)

LambdaBlock <- "{" (LambdaParaList+ "->")?  LambdaStatement? "}"

LambdaParaList <- LambdaParaList "," LambdaParameter | LambdaParameter
LambdaParameter <- Identifier TypeAnnotation?

LambdaStatement <- LambdaStatement ";" Expression | Expression

Expression <- Identifier / Literature / Expression ("+" / "-" / "*" / "/") Expression 
Binding <- "let" Identifier TypeAnnotation? "=" Expression

TypeAnnotation <- ":" Type

Type <- TypeApplication / TypeArrow / TypeArray / TypeSymbol
TypeSymbol <- Identifier
TypeApplication <- Type+
TypeArrow <- TypeArrow? "->" Type / Type "->" Type
TypeArray <- '[' Type ']'

Identifier <- IdentifierToken (IdentifierToken / [0-9])*
IdentifierToken <- [a-z] / [A-Z] / '_'

Literature <- Number | StringLit

Number <- '-'? (NumberInteger / NumberFloat / NumberHex)
NumberToken <- [0-9]
NumberInteger <- NumberToken+
NumberFloat <- NumberToken+ ('.' NumberToken+)?
NumberHex <- '0' ('x' / 'X') (NumberToken / [a-f] / [A-F])+

StringLit <- '"' StringLitToken* '"'
StringLitToken <- '\' '"' / [a-z] / [A-Z] / [0-9] / '!@#$%^&*()_+|-=\`~{}[],./<>?;:'

WhiteSpace <- WhiteSpaceToken*
WhiteSpaceToken <- ' ' / '\n'

-}

-- A general abstract syntax tree for multiple purpose
data LanguageSyntax name ext =
  LanguageSyntax
  deriving (Show, Eq)

-- TODO, We may need some kind of context being shared among different translation stages.
-- To save information of
--  - function definition with type variable involved,
--  - or Types from different translation unit (the unit here is Language Module).
-- Since we can't nest all AST type information into LLVM region.
-- Semantic analysis should build up context.
-- Code generation should have this context as the guide.
-- maybe a intermediate representation should be introduced? e.g. a file stored every information of a module, like symbol, type definition, constant value and so on.
data LanguageModuleContext
