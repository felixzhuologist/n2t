module Syntax where

import Data.List.NonEmpty

type Name = String

-- Program structure
data Program = Class Name [AttrDecl] [MethodDecl]

type AttrDecl = (JScope, JType, (NonEmpty Name)) -- todo

type MethodDecl = (DeclType, JType, Name, [Param], MethodBody)

data JType
  = JInt
  | JChar
  | JBool
  | JObject

data JScope = Static | Field

data DeclType = ConstructorDecl | FunctionDecl | MethodDecl

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
  | Var Name
  | ArrayIndex Name Expr
  | FuncCall
  | BoolVal Bool
  | Null
  | This
  | Uno Uop Expr
  | Duo Bop Expr Expr
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
  = Neg
  | Not
  deriving (Eq, Ord, Show)

data FuncCall
  = Func Name [Expr]
  | Method Name Name [Expr] -- class.method(expr)
  deriving (Eq, Ord, Show)

