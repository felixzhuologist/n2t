{-
This code should be mostly correct for expressions/statements/arrays but
still needs some replumbing for classes:
- Environment needs to keep track of some internal state in order to create unique
labels.
- Environment needs to also have a list of subroutines to look up. Should be able to
get whether a subroutine is a function, constructor, or method by looking it up.
- compile() needs to store the subroutines into the Environment
- method codegen needs to push the class as the first argument to the function
if the function is a method
-}
module Codegen where

import Control.Applicative
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import Text.PrettyPrint
import System.Environment

import Syntax
import Parser

-- TODO: enforce specific JKind using DataKinds?
type SymbolTable = [(Name, JType, Segment)]
-- TODO: might be cleaner to make this a data type using record syntax
type Environment = (SymbolTable, -- locals
                    SymbolTable, -- arguments
                    SymbolTable, -- fields
                    SymbolTable) -- statics

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
  | POINTER
  deriving (Eq, Ord, Show)

instance ConstPP Segment where
  constPP STATIC = text "static"
  constPP LCL = text "local"
  constPP ARG = text "argument"
  constPP THIS = text "this"
  constPP THAT = text "that"
  constPP CONST = text "constant"
  constPP TEMP = text "temp"
  constPP POINTER = text "pointer"

instance ConstPP Bop where
  constPP Plus = text "add"
  constPP Minus = text "sub"
  constPP Times = (text "call Math.multiply 2")
  constPP Divide = (text "call Math.divide 2")
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
  pp (LetArray arr index val) env =
    (pp (Var arr) env) $$ -- push *arr + index onto stack (i.e. base addr + offset)
    (pp index env) $$
    (text "add") $$ 
    (pp val env) $$ -- push val onto stack
    (pop (TEMP, 0)) $$ -- push val into temp register
    (pop (POINTER, 1)) $$ -- push dest addr into THAT register
    (push TEMP (char '0')) $$ 
    (pop (THAT, 0)) -- store val into addr in THAT
  -- TODO: use state monad to get incrementing labels
  pp (If cond block) env =
    (pp (Unary Not cond) env) $$
    ifgoto "IF_END" $$
    (ppBlock block env) $$
    label "IF_END"
  pp (IfElse cond b1 b2) env = 
    (pp cond env) $$
    ifgoto "IF_TRUE" $$
    goto "IF_FALSE" $$
    label "IF_TRUE" $$
    (ppBlock b1 env) $$
    goto "IFELSE_END" $$
    label "IF_FALSE" $$
    (ppBlock b2 env) $$
    label "IFELSE_END"
  pp (While cond body) env =
    label "WHILE_EXP" $$
    (pp (Unary Not cond) env) $$
    ifgoto "WHILE_END" $$
    (ppBlock body env) $$
    goto "WHILE_EXP" $$
    label "WHILE_END"
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
  pp (BoolVal True) env = pp (Unary Not (IntVal 0)) env
  pp (BoolVal False) _ = push CONST (char '0')
  pp (Var s) env = let (segment, i) = getIdentifier env s in push segment (int i)
  pp (ArrayIndex arr index) env =
    (pp (Var arr) env) $$ -- push *arr + index onto stack (i.e. base addr + offset)
    (pp index env) $$
    (text "add") $$ 
    pop (POINTER, 1) $$
    push THAT (char '0')
  pp (Call f) env = pp f env
  pp Null _ = push CONST (char '0')
  pp This _ = push POINTER (char '0')
  pp (Unary op e) env = (pp e env) $$ (constPP op)
  pp (Binary op e1 e2) env = (pp e1 env) $$ (pp e2 env) $$ (constPP op)

instance PP FuncCall where
  pp (Func f args) env = fcall f args env
  pp (Method c m args) env = let  
    (let (seg, idx) = getIdentifier env c in push seg (int idx)) $$
    fcall (c ++ "." ++ m) args env

fcall :: String -> [Expr] -> Environment -> Doc
fcall func args env = pushArgs $$ funcCall where
  pushArgs = vcat $ map (flip pp env) args
  funcCall = (text "call") <+> (text func) <+> (int $ length args)

-- TODO: change method to data to be able to implement pp
ppMethod :: Name -> MethodDecl -> Environment -> Doc
ppMethod className (c, t, name, _, (vars, stmts)) env@(locals, _, _, _) =
  (text "function" <+> (text className <> char '.' <> text name) <+> (int $ length locals)) $$
  doSetup c env $$
  (vcat $ map (flip pp env) stmts) $$
  doTeardown t

doSetup :: DeclType -> Environment -> Doc
doSetup FunctionDecl _ = Text.PrettyPrint.empty
doSetup MethodDecl _ = (push ARG (char '0')) $$ (pop (POINTER, 0)) -- load THIS
doSetup ConstructorDecl (_, _, fields, statics) = -- alloc space for the new object and load THIS
  (push CONST (int $ length fields + length statics)) $$
  (text "call Memory.alloc 1") $$
  (pop (POINTER, 0))

doTeardown :: JType -> Doc
doTeardown _ = Text.PrettyPrint.empty

compile :: Program -> Doc
compile (Class name attrs methods) = vcat $ map (compileMethod name classSymbols) methods where
  classSymbols = (getSymbols Field attrs, getSymbols Static attrs)

compileMethod :: Name -> (SymbolTable, SymbolTable) -> MethodDecl -> Doc
compileMethod className (fields, statics) subroutine@(_, _, _, args, (lcls, _)) =
  ppMethod className subroutine symbolTable where
    symbolTable = (locals, arguments, fields, statics)
    arguments = map (\(t, n) -> (n, t, ARG)) args
    locals = lcls >>= getEntries
    getEntries (t, vars) = NonEmpty.toList $ NonEmpty.map (\v -> (v, t, LCL)) vars

getSymbols :: JScope -> [AttrDecl] -> SymbolTable
getSymbols k attrs = (filter matchK attrs) >>= getEntries where
  matchK :: AttrDecl -> Bool
  matchK (k', _, _) = k == k'
  getEntries :: AttrDecl -> [(Name, JType, Segment)] -- unpack vars from a line of decls
  getEntries (k, t, vars) = NonEmpty.toList $ NonEmpty.map (\v -> (v, t, jscopeToSegment k)) vars

jscopeToSegment :: JScope -> Segment
jscopeToSegment Static = STATIC
jscopeToSegment Field = THIS

main :: IO ()
main = do
  [inpath, outpath] <- getArgs
  parsed <- parseFromFile program inpath
  case parsed of
    (Left _) -> putStrLn "parse error"
    (Right prog) -> writeFile outpath (render $ compile prog)
