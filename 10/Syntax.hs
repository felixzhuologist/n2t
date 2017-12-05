module Syntax where

import Data.List.NonEmpty

type Name = String

-- Program structure
data Program = Class Name [AttrDecl] [MethodDecl]
  deriving (Eq, Ord, Show)

type AttrDecl = (JScope, JType, (NonEmpty Name)) -- todo

type MethodDecl = (DeclType, JType, Name, [Param], MethodBody)

data JType
  = JInt
  | JChar
  | JBool
  | JObject
  | Void
  deriving (Eq, Ord, Show)

data JScope = Static | Field
  deriving (Eq, Ord, Show)

data DeclType = ConstructorDecl | FunctionDecl | MethodDecl
  deriving (Eq, Ord, Show)

type Param = (JType, Name)

type MethodBody = ([VarDecl], [Statement])

type VarDecl = (JType, (NonEmpty Name))

-- Statements
data Statement
  = Let Name Expr
  | LetArray Name Expr Expr
  | If Expr [Statement]
  | IfElse Expr [Statement] [Statement]
  | While Expr [Statement]
  | Do FuncCall
  | Return
  | ReturnVal Expr 
  deriving (Eq, Ord, Show)

-- Expressions
data Expr
  = IntVal Int
  | StrVal String
  | BoolVal Bool
  | Var Name
  | ArrayIndex Name Expr
  | Call FuncCall
  | Null
  | This
  | Unary Uop Expr
  | Binary Bop Expr Expr
  deriving (Eq, Ord, Show)

data Bop
  = Plus
  | Minus
  | Times
  | Divide
  | Intersect
  | Union
  | Lt
  | Gt
  | Eq
  deriving (Eq, Ord, Show)

data Uop
  = Neg -- not used in parsing/AST but used for code gen
  | Not
  deriving (Eq, Ord, Show)

data FuncCall
  = Func Name [Expr]
  | Method Name Name [Expr] -- class.method(expr)
  deriving (Eq, Ord, Show)

