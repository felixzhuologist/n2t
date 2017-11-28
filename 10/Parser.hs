module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax


binary s f = Ex.Infix (reservedOp s >> return (Binary f)) Ex.AssocLeft
unary s f = Ex.Prefix (reservedOp s >> return (Unary f))

table = [[unary "-" Neg,
          unary "~" Not]
        ,[binary "*" Times,
          binary "/" Divide]
        ,[binary "+" Plus,
          binary "-" Minus]]

expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor = try intval
      <|> try strval
      <|> try boolval
      <|> try array
      -- <|> try call
      <|> try nullval
      <|> try this
      <|> var
      <|> parens expr

-- expr terms
intval :: Parser Expr
intval = fmap (IntVal . fromInteger) integer

strval :: Parser Expr
strval = fmap StrVal str

boolval :: Parser Expr
boolval = try true <|> try false where
  true = fmap (const $ BoolVal True) (reserved "true")
  false = fmap (const $ BoolVal False) (reserved "false")

var :: Parser Expr
var = fmap Var identifier

array :: Parser Expr
array = do
  name <- identifier
  index <- brackets expr
  return $ ArrayIndex name index

call :: Parser Expr
call = undefined

nullval :: Parser Expr
nullval = fmap (const Null) (reserved "null")

this :: Parser Expr
this = fmap (const This) (reserved "this")

{--
function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
--}