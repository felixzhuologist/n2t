module Tests where

import Data.List
import Data.List.NonEmpty
import Data.Maybe

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.PrettyPrint

import Test.HUnit

import Syntax
import Parser
import Codegen

main :: IO ()
main = do
  _ <- runTestTT (TestList [
        tParseFactor, tParseExpr,
        tParseStmt, tParseBlock,
        tAttrs, tMethods,
        tParseFile,
        tGenFactors, tEnv])
  return ()

testp :: Parser a -> String -> Either ParseError a
testp p s = parse p "" s

-- expr parsing tests
tParseFactor :: Test
tParseFactor = "parse factor" ~: TestList [
                  "1234" ~: testp factor "1234" ~?= Right (IntVal 1234),
                  "-007" ~: testp factor "-007" ~?= Right (IntVal (-7)),
                  "_protected" ~: testp factor "_protected" ~?= Right (Var "_protected"),
                  "\"somestr\"" ~: testp factor "\"somestr\"" ~?= Right (StrVal "somestr"),
                  "A[3]" ~: testp factor "A[3]" ~?= Right (ArrayIndex "A" (IntVal 3)),
                  "null" ~: testp factor "null" ~?= Right Null,
                  "this" ~: testp factor "this" ~?= Right This,
                  "add(a, 3)" ~: testp factor "add(a, 3)" ~?= Right (Call (Func "add" [Var "a", IntVal 3])),
                  "Dog.eat(\"apple\")" ~: testp factor "Dog.eat(\"apple\")" ~?= Right (Call (Method "Dog" "eat" [StrVal "apple"]))]

tParseExpr :: Test
tParseExpr = "parse expr" ~: TestList [
                "~true" ~: testp expr "~true" ~?= Right (Unary Not (BoolVal True)),
                "1 + (2 + 3)" ~: testp expr "1 + (2 + 3)" ~?= Right (Binary Plus (IntVal 1) (Binary Plus (IntVal 2) (IntVal 3))),
                "10 - 2 * 5" ~: testp expr "10 - 2 * 5" ~?= Right (Binary Minus (IntVal 10) (Binary Times (IntVal 2) (IntVal 5))),
                "(10 - 2) * 5" ~: testp expr "(10 - 2) * 5" ~?= Right (Binary Times (Binary Minus (IntVal 10) (IntVal 2)) (IntVal 5))]

-- statement parsing tests
tParseStmt :: Test
tParseStmt = "parse statements" ~: TestList [
                "let" ~: testp statement "let _id = \"hi\";" ~?= Right (Let "_id" (StrVal "hi")),
                "let a[3 + 4] = true;" ~: testp statement "let a[3 + 4] = true;" ~?= Right (LetArray "a" (Binary Plus (IntVal 3) (IntVal 4)) (BoolVal True)),
                "return;" ~: testp statement "return;" ~?= Right Return,
                "return 3;" ~: testp statement "return 3;" ~?= Right (ReturnVal (IntVal 3)),
                "do f(3);" ~: testp statement "do f(3);" ~?= Right (Do (Func "f" [(IntVal 3)]))]

tParseBlock :: Test
tParseBlock = "pares block" ~: TestList [
                "if" ~: testp block "if (a = 0) {return 3;}" ~?= Right [If (Binary Eq (Var "a") (IntVal 0)) [(ReturnVal (IntVal 3))]],
                "ifelse" ~: testp block "if (true) {return;} else {let a = 1;}" ~?= Right [IfElse (BoolVal True) [Return] [Let "a" (IntVal 1)]],
                "while" ~: testp block "while (i > 0) {let a[i] = 0; let i = i - 1;}" ~?= Right [While (Binary Gt (Var "i") (IntVal 0)) [LetArray "a" (Var "i") (IntVal 0), Let "i" (Binary Minus (Var "i") (IntVal 1))]]]

-- program structure parsing tests
tAttrs = "parse attributes" ~: TestList [
          "static int a, b, c;" ~: testp attr "static int a, b, c;" ~?= Right (Static, JInt, (fromList ["a", "b", "c"])),
          "field Dog my_dog;" ~: testp attr "field Dog my_dog;" ~?= Right (Field, JObject, (fromList ["my_dog"]))]

add_method :: MethodDecl
add_method
 = (ConstructorDecl,
    Void,
    "add",
    [(JInt, "a"), (JInt, "b")],
    ([(JInt, fromList ["c"])], [Return]))
tMethods = "parse method defs" ~: TestList [
              "method" ~: testp method "constructor void add(int a, int b) {var int c; return;}" ~?= Right add_method]

-- e2e parsing test
squareGame :: Program
squareGame =
  Class
    "SquareGame"
    [(Field, JObject, fromList ["square"]),
     (Field, JInt, fromList ["direction"])]
    [(ConstructorDecl, JObject , "new", [], (
        [],
        [Let "square" (Call (Method "Square" "new" [IntVal 0,IntVal 0,IntVal 30])),
         Let "direction" (IntVal 0),ReturnVal This])),
     (MethodDecl, Void, "dispose", [], (
        [],
        [Do (Method "square" "dispose" []),
         Do (Method "Memory" "deAlloc" [This]),
         Return])),
     (MethodDecl, Void, "moveSquare", [], (
        [],
        [If (Binary Eq (Var "direction") (IntVal 1)) [Do (Method "square" "moveUp" [])],
         If (Binary Eq (Var "direction") (IntVal 2)) [Do (Method "square" "moveDown" [])],
         If (Binary Eq (Var "direction") (IntVal 3)) [Do (Method "square" "moveLeft" [])],
         If (Binary Eq (Var "direction") (IntVal 4)) [Do (Method "square" "moveRight" [])],
         Do (Method "Sys" "wait" [IntVal 5]),Return])),
     (MethodDecl, Void, "run", [], (
      [(JChar,"key" :| []),
       (JBool,"exit" :| [])],
      [Let "exit" (BoolVal False),
       While (Unary Not (Var "exit"))
        [While (Binary Eq (Var "key") (IntVal 0))
          [Let "key" (Call (Method "Keyboard" "keyPressed" [])),
           Do (Func "moveSquare" [])],
           If (Binary Eq (Var "key") (IntVal 81)) [Let "exit" (BoolVal True)],
           If (Binary Eq (Var "key") (IntVal 90)) [Do (Method "square" "decSize" [])],
           If (Binary Eq (Var "key") (IntVal 88)) [Do (Method "square" "incSize" [])],
           If (Binary Eq (Var "key") (IntVal 131)) [Let "direction" (IntVal 1)],
           If (Binary Eq (Var "key") (IntVal 133)) [Let "direction" (IntVal 2)],
           If (Binary Eq (Var "key") (IntVal 130)) [Let "direction" (IntVal 3)],
           If (Binary Eq (Var "key") (IntVal 132)) [Let "direction" (IntVal 4)],
           While (Unary Not (Binary Eq (Var "key") (IntVal 0)))
            [Let "key" (Call (Method "Keyboard" "keyPressed" [])),
             Do (Func "moveSquare" [])]],
       Return]))]

tParseFile :: Test
tParseFile = "parse file" ~: TestList ["SquareGame.jack" ~: p "Square/SquareGame.jack" squareGame] where
  p file ast = do
    result <- parseFromFile program file
    case result of
      (Left _) -> assert False
      (Right ast') -> assert (ast == ast')

-- codegen tests
unlines' :: [String] -> String
unlines' = intercalate "\n"

tGenFactors :: Test
tGenFactors = "codegen without symbol table" ~: TestList [
                "add" ~: (render $ pp (Binary Plus (IntVal 3) (IntVal 5)) emptyEnv) ~?= (unlines' ["push constant 3", "push constant 5", "add"]),
                "mul" ~: (render $ pp (Binary Times (IntVal 1) (IntVal 2)) emptyEnv) ~?= (unlines' ["push constant 1", "push constant 2", "call Math.multiply"]),
                "null" ~: (render $ pp Null emptyEnv) ~?= "push constant 0",
                "true" ~: (render $ pp (BoolVal True) emptyEnv) ~?= (unlines' ["push constant 1", "neg"]),
                "false" ~: (render $ pp (BoolVal False) emptyEnv) ~?= "push constant 0",
                "function call" ~: (render $ pp (Call $ Func "f" [(IntVal 9), (IntVal 3)]) emptyEnv) ~?= (unlines' ["push constant 9", "push constant 3", "call f 2"])]

tEnv :: Test
testEnv :: Environment
testEnv = ([("a", JInt, LCL)],
           [("a", JInt, ARG), ("b", JInt, ARG)],
           [("a", JInt, THIS), ("c", JInt, THIS)],
           [("d", JInt, STATIC)])

tEnv = "test getting var from env" ~: TestList [
          "tries local first" ~: getIdentifier testEnv "a" ~?= (LCL, 0),
          "indexing works" ~: getIdentifier testEnv "c" ~?= (THIS, 1),
          "gets static" ~: getIdentifier testEnv "d" ~?= (STATIC, 0)]
