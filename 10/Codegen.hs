module Codegen where

import Text.PrettyPrint

import Syntax
import Parser

class PP a where
  pp :: a -> Doc

data Segment = STATIC
             | LCL
             | ARG
             | THIS
             | THAT
             | CONST

instance PP Segment where
  pp STATIC = text "static"
  pp LCL = text "local"
  pp ARG = text "argument"
  pp THIS = text "this"
  pp THAT = text "that"
  pp CONST = text "constant"

push :: Segment -> Doc -> Doc
push seg val = (text "push") <+> (pp seg) <+> val

pop :: Segment -> Doc
pop seg = (text "pop") <+> (pp seg)

instance PP Statement where
  pp (Let var val) = undefined
  pp (If cond block) = undefined
  pp (IfElse cond b1 b2) = undefined
  pp (While cond body) = undefined
  pp (Do f) = pp f
  pp Return = (push CONST (char '0')) $$ (text "return")
  pp (ReturnVal e) = (pp e) $$ (text "return")

instance PP Expr where
  pp (IntVal n) 
    | n >= 0 = push CONST (int n)
    | otherwise = (push CONST (int n)) $$ (pp Neg)
  pp (StrVal s) = push CONST (text s)
  pp (BoolVal True) = push CONST (char '0')
  pp (BoolVal False) = push CONST (char '1')
  pp (Var s) = undefined -- need symbol table?
  pp (ArrayIndex arr index) = undefined
  pp (Call f) = pp f
  pp Null = push CONST (char '0')
  pp (Unary op e) = (pp e) $$ (pp op)
  pp (Binary op e1 e2) = (pp e1) $$ (pp e2) $$ (pp op)

instance PP Bop where
  pp Plus = text "add"
  pp Minus = text "sub"
  pp Times = undefined
  pp Divide = undefined 
  pp Intersect = text "and"
  pp Union = text "or"
  pp Lt = text "lt"
  pp Gt = text "gt"
  pp Eq = text "eq"

instance PP Uop where 
  pp Neg = text "neg"
  pp Not = text "not"

instance PP FuncCall where
  pp (Func f args) = fcall f args
  pp (Method c m args) = fcall (c ++ "." ++ m) args

fcall :: String -> [Expr] -> Doc
fcall func args = pushArgs $$ funcCall where
  pushArgs = vcat $ map pp args
  funcCall = (text "call") <+> (text func) <+> (int $ length args)
