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
  | TEMP
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

pop :: (Segment, Int) -> Doc
pop (seg, index) = (text "pop") <+> (constPP seg) <+> (int index)

ifgoto :: String -> Doc
ifgoto l = (text "if-goto") <+> (text l)

goto :: String -> Doc
goto l = (text "goto") <+> (text l)

label :: String -> Doc
label l = (text "label") <+> (text l)

-- should probably make Block a newtype to implement pp
ppBlock :: [Statement] -> Environment -> Doc
ppBlock stmts env = vcat $ map (flip pp env) stmts

instance PP Statement where
  pp (Let var val) env = (pp val env) $$ (pop $ getIdentifier env var)
  -- TODO: generate unique labels
  pp (If cond block) env =
    (pp (Unary Neg cond) env) $$
    ifgoto "IF_END" $$
    (ppBlock block env) $$
    label "IF_END"
  pp (IfElse cond b1 b2) env = 
    (pp cond env) $$
    ifgoto "IF_TRUE" $$
    (ppBlock b2 env) $$
    goto "IFELSE_END" $$
    label "IF_TRUE" $$
    (ppBlock b1 env) $$
    label "IFELSE_END"
  pp (While cond body) env =
    label "LOOP_START" $$
    (pp (Unary Neg cond) env) $$
    ifgoto "LOOP_END" $$
    (ppBlock body env) $$
    goto "LOOP_START" $$
    label "LOOP_END"
  -- when returning empty value, push dummy return value in return codegen and
  -- pop the dummy value in Do func codegen
  pp (Do f) env = (pp f env) $$ (pop (TEMP, 0))
  pp Return _ =  (push CONST (char '0')) $$ text "return"
  pp (ReturnVal e) env = (pp e env) $$ (text "return")

instance PP Expr where
  pp (IntVal n) env
    | n >= 0 = push CONST (int n)
    | otherwise = (push CONST (int n)) $$ (constPP Neg)
  pp (StrVal s) _ = undefined
  pp (BoolVal True) env = pp (Unary Neg (IntVal 1)) env
  pp (BoolVal False) _ = push CONST (char '0')
  pp (Var s) env = let (segment, i) = getIdentifier env s in push segment (int i)
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
