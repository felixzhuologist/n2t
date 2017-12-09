module Codegen where

import Control.Applicative
import Data.Maybe
import Text.PrettyPrint

import Syntax
import Parser

{-
When generating code the only state we need to keep track of is the current
class symbol table and the current method symbol table. To make things simpler,
instead of generating code and updating the symbol table as we go using a state monad, we get the
symbol table from the class/subroutine AST node first, and then call pp to do
codegen afterwards. This is also easy because of the Jack grammar which already
groups declarations together for us.
-}
-- TODO: enforce specific JKind using DataKinds?
type Environment = ([(Name, JType, Segment)], -- locals
                    [(Name, JType, Segment)], -- arguments
                    [(Name, JType, Segment)], -- fields
                    [(Name, JType, Segment)]) -- statics

emptyEnv :: Environment
emptyEnv = ([], [], [], [])

-- get the segment and index for a variable from the Environment
getIdentifier :: Environment -> Name -> (Segment, Int)
getIdentifier (lcl, arg, this, static) v = 
  case foldl (<|>) Nothing (map (findByKey v (\(x, _, _) -> x)) [lcl, arg, this, static]) of
    Nothing -> error "undefined symbol" -- TODO
    (Just (i, (_, _, segment))) -> (segment, i)


-- find by key f and return both the element and the index if it exists
findByKey :: (Eq a) => a -> (b -> a) -> [b] -> Maybe (Int, b)
findByKey = findByKey' 0 where
  findByKey' _ _ _ [] = Nothing
  findByKey' n x f (x':xs)
    | x == f x' = Just (n, x')
    | otherwise = findByKey' (n + 1) x f xs

class PP a where
  pp :: a -> Environment -> Doc

class ConstPP a where
  constPP :: a -> Doc

data Segment
  = STATIC
  | LCL
  | ARG
  | THIS
  | THAT
  | CONST
  deriving (Eq, Ord, Show)

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
  pp (Let var val) env = undefined
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
