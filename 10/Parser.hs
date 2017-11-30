module Parser where

import Control.Monad (guard)

import Data.List.NonEmpty
import Data.Maybe

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax


binary s f = Ex.Infix (reservedOp s >> return (Binary f)) Ex.AssocLeft
unary s f = Ex.Prefix (reservedOp s >> return (Unary f))

-- todo: separate arithmetic vs bool exprs
table = [[unary "-" Neg,
          unary "~" Not]
        ,[binary "*" Times,
          binary "/" Divide]
        ,[binary "+" Plus,
          binary "-" Minus]
        ,[binary "&" Intersect,
          binary "|" Union,
          binary "<" Lt,
          binary ">" Gt,
          binary "=" Eq]]

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

-- expressions
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

call' :: Parser FuncCall
call' = undefined

nullval :: Parser Expr
nullval = fmap (const Null) (reserved "null")

this :: Parser Expr
this = fmap (const This) (reserved "this")

-- statements
block :: Parser [Statement]
block = many statement

statement :: Parser Statement
statement = try letArrayStmt
        <|> letStmt
        <|> try ifElseStmt
        <|> ifStmt
        <|> whileStmt
        <|> doStmt
        <|> try returnValStmt
        <|> returnStmt

letStmt :: Parser Statement
letStmt = do
  reserved "let"
  varname <- identifier
  reservedOp "="
  val <- expr
  semi
  return $ Let varname val

letArrayStmt :: Parser Statement
letArrayStmt = do
  reserved "let"
  (ArrayIndex varname index) <- array
  reservedOp "="
  val <- expr
  semi
  return $ LetArray varname index val

ifStmt :: Parser Statement
ifStmt = do
  reserved "if"
  cond <- expr
  body <- braces block
  return $ If cond body

ifElseStmt :: Parser Statement
ifElseStmt = do
  reserved "if"
  cond <- expr
  ifTrue <- braces block
  reserved "else"
  ifFalse <- braces block
  return $ IfElse cond ifTrue ifFalse

whileStmt :: Parser Statement
whileStmt = do
  reserved "while"
  cond <- expr
  body <- braces block
  return $ While cond body

doStmt :: Parser Statement
doStmt = do
  reserved "do"
  func <- call'
  semi
  return $ Do func

returnStmt :: Parser Statement
returnStmt = reserved "return" >> semi >> (return Return)

returnValStmt :: Parser Statement
returnValStmt = do
  reserved "return"
  val <- expr
  semi
  return $ ReturnVal val

-- program structure
program :: Parser Program
program = do
  reserved "class"
  className <- identifier
  (attrs, methods) <- braces classBody
  return $ Class className attrs methods

classBody :: Parser ([AttrDecl], [MethodDecl])
classBody = do
  attrs <- many attr
  methods <- many method
  return $ (attrs, methods)

attr :: Parser AttrDecl
attr = do
  scope <- attrScope
  t <- jType
  attrs <- fmap nonEmpty $ many identifier
  guard $ isJust attrs
  return (scope, t, fromJust attrs)

method :: Parser MethodDecl
method = do
  mtype <- methodType
  rtype <- jType
  name <- identifier
  params <- parens $ commaSep param
  body <- braces $ methodBody
  return (mtype, rtype, name, params, body)

jType :: Parser JType
jType = try jint <|> try jchar <|> try jbool <|> jobj where
  jint = fmap (const JInt) (reserved "int")
  jchar = fmap (const JChar) (reserved "char")
  jbool = fmap (const JBool) (reserved "bool")
  jobj = fmap (const JObject) identifier

attrScope :: Parser JScope
attrScope = try static <|> try field where
  static = fmap (const Static) (reserved "static")
  field = fmap (const Field) (reserved "field")

methodType :: Parser DeclType
methodType = try constructor <|> try function <|> method where
  constructor = fmap (const ConstructorDecl) (reserved "constructor")
  function = fmap (const FunctionDecl) (reserved "function")
  method = fmap (const MethodDecl) (reserved "method")

param :: Parser Param
param = do
  t <- jType
  arg <- identifier
  return (t, arg)

methodBody :: Parser MethodBody
methodBody = do
  locals <- many varDecl
  body <- block
  return (locals, body)

varDecl :: Parser VarDecl
varDecl = do
  t <- jType
  vars <- fmap nonEmpty $ commaSep identifier
  guard $ isJust vars
  semi
  return (t, fromJust vars)

{--
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    semi
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
--}