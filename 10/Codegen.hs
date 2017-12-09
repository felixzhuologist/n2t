module Codegen where

import Text.PrettyPrint

import Syntax
import Parser

data VarKind = Arg | Local

type Environment = (ClassSymbols, MethodSymbols)
-- contains statics and fields for a class
type ClassSymbols = [(Name, JType, JScope)]
-- contains arguments and vars for a method
type MethodSymbols = [(Name, JType, VarKind)]

emptyEnv :: Environment
emptyEnv = ([], [])

class PP a where
  pp :: a -> Environment -> Doc

class ConstPP a where
  constPP :: a -> Doc

data Segment = STATIC
             | LCL
             | ARG
             | THIS
             | THAT
             | CONST

instance ConstPP Segment where
  constPP STATIC = text "static"
  constPP LCL = text "local"
  constPP ARG = text "argument"
  constPP THIS = text "this"
  constPP THAT = text "that"
  constPP CONST = text "constant"

instance ConstPP Bop where
  constPP Plus = text "add"
  constPP Minus = text "sub"
  constPP Times = (text "call Math.multiply")
  constPP Divide = (text "call Math.divide")
  constPP Intersect = text "and"
  constPP Union = text "or"
  constPP Lt = text "lt"
  constPP Gt = text "gt"
  constPP Eq = text "eq"

instance ConstPP Uop where 
  constPP Neg = text "neg"
  constPP Not = text "not"

push :: Segment -> Doc -> Doc
push seg val = (text "push") <+> (constPP seg) <+> val

instance PP Statement where
  pp (Let var val) _ = undefined
  pp (If cond block) _ = undefined
  pp (IfElse cond b1 b2) _ = undefined
  pp (While cond body) _ = undefined
  pp (Do f) env = pp f env
  pp Return _ = text "return"
  pp (ReturnVal e) env = (pp e env) $$ (text "return")

instance PP Expr where
  pp (IntVal n) env
    | n >= 0 = push CONST (int n)
    | otherwise = (push CONST (int n)) $$ (constPP Neg)
  pp (StrVal s) _ = undefined
  pp (BoolVal True) env = pp (Unary Neg (IntVal 1)) env
  pp (BoolVal False) _ = push CONST (char '0')
  pp (Var s) _ = undefined -- need symbol table?
  pp (ArrayIndex arr index) _ = undefined
  pp (Call f) env = pp f env
  pp Null _ = push CONST (char '0')
  pp (Unary op e) env = (pp e env) $$ (constPP op)
  pp (Binary op e1 e2) env = (pp e1 env) $$ (pp e2 env) $$ (constPP op)

instance PP FuncCall where
  pp (Func f args) env = fcall f args env
  pp (Method c m args) env = fcall (c ++ "." ++ m) args env

fcall :: String -> [Expr] -> Environment -> Doc
fcall func args env = pushArgs $$ funcCall where
  pushArgs = vcat $ map (flip pp env) args
  funcCall = (text "call") <+> (text func) <+> (int $ length args)
