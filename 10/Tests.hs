module Tests where

import Text.Parsec
import Text.Parsec.String (Parser)

import Test.HUnit

import Syntax
import Parser

main :: IO ()
main = do
  _ <- runTestTT (TestList [tParseFactor, tParseExpr])
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
                  "this" ~: testp factor "this" ~?= Right This]

tParseExpr :: Test
tParseExpr = "parse expr" ~: TestList [
                "~true" ~: testp expr "~true" ~?= Right (Unary Not (BoolVal True)),
                "1 + (2 + 3)" ~: testp expr "1 + (2 + 3)" ~?= Right (Binary Plus (IntVal 1) (Binary Plus (IntVal 2) (IntVal 3))),
                "10 - 2 * 5" ~: testp expr "10 - 2 * 5" ~?= Right (Binary Minus (IntVal 10) (Binary Times (IntVal 2) (IntVal 5))),
                "(10 - 2) * 5" ~: testp expr "(10 - 2) * 5" ~?= Right (Binary Times (Binary Minus (IntVal 10) (IntVal 2)) (IntVal 5))]
