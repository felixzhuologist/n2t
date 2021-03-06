module Lexer where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "-", "*", "/", "&", "|", "<", ">", "=", "-", "~"]
    names = ["class", "constructor", "function", "method", "field", "static",
             "var", "int", "char", "boolean", "void", "true", "false", "null",
             "this", "let", "do", "if", "else", "while", "return"]
    style = emptyDef {
              Tok.identStart = letter <|> char '_',
              Tok.identLetter = alphaNum <|> char '_',
              Tok.commentLine = "//",
              Tok.commentStart = "/**",
              Tok.commentEnd = "*/",
              Tok.reservedOpNames = ops,
              Tok.reservedNames = names
            }

integer :: Parser Integer
integer = Tok.integer lexer

str :: Parser String
str = Tok.stringLiteral lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

semi :: Parser String
semi = Tok.semi lexer

dot :: Parser String
dot = Tok.dot lexer
