module Tests where

import Data.List.NonEmpty
import Data.Maybe

import Text.Parsec
import Text.Parsec.String (Parser)

import Test.HUnit

import Syntax
import Parser

main :: IO ()
main = do
  _ <- runTestTT (TestList [
        tParseFactor, tParseExpr,
        tParseStmt, tParseBlock,
        tAttrs, tMethods])
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
          "static int a, b, c;" ~: testp attr "static int a, b, c;" ~?= Right (Static, JInt, (fromJust $ nonEmpty ["a", "b", "c"])),
          "field Dog my_dog;" ~: testp attr "field Dog my_dog;" ~?= Right (Field, JObject, (fromJust $ nonEmpty ["my_dog"]))]

add_method :: MethodDecl
add_method
 = (ConstructorDecl,
    Void,
    "add",
    [(JInt, "a"), (JInt, "b")],
    ([(JInt, fromJust $ nonEmpty ["c"])], [Return]))
tMethods = "parse method defs" ~: TestList [
              "method" ~: testp method "constructor void add(int a, int b) {var int c; return;}" ~?= Right add_method]
