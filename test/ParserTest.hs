{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ParserTest where

import           Ast
import           Parser                   (parseScheme)
import           Test.Framework
import           Test.Framework.TestTypes (Assertion)


parsedErr :: String -> String -> Assertion
parsedErr expected input = assertEqual (Left expected) (parseScheme input)

parsedOk :: SchemeAst -> String -> Assertion
parsedOk expected input = assertEqual (Right expected) (parseScheme input)


test_emptyInput :: Assertion
test_emptyInput = parsedErr "empty input" ""

test_number :: Assertion
test_number = do
    parsedOk (Number 5) "5"
    parsedOk (Number 5) " 5"
    parsedOk (Number 5) "5  "
    parsedOk (Number 5) "  5 "
    parsedOk (Number 5) "  5 "
    parsedOk (Number (1585)) "1585"
    parsedOk (Number (1585)) "  1585   "

test_bool :: Assertion
test_bool = do
    parsedOk (Bool True) "#t"
    parsedOk (Bool True) " #t"
    parsedOk (Bool True) "#t "
    parsedOk (Bool True) " #t "
    parsedOk (Bool False) "#f"
    parsedOk (Bool False) " #f"
    parsedOk (Bool False) "#f "
    parsedOk (Bool False) " #f "

test_atom :: Assertion
test_atom = do
    parsedOk (Atom "name") "name"
    parsedOk (Atom "name") "    name  "
    parsedOk (Atom "+") "+"
    parsedOk (Atom "+") "  +   "
    parsedOk (Atom "call-with-current-continuation") "call-with-current-continuation"
    parsedOk (Atom "call-with-current-continuation") "  call-with-current-continuation   "

test_string :: Assertion
test_string = do
    parsedOk (String "str") "\"str\""
    parsedOk (String "str") "   \"str\"   "
    parsedOk (String "str1 str2") "\"str1 str2\""
    parsedOk (String "str1 str2") "  \"str1 str2\"   "
    parsedOk (String "str1   str2") "  \"str1   str2\"  "

test_list :: Assertion
test_list = do
    parsedOk (List []) "()"
    parsedOk (List []) "   ()   "
    parsedOk (List []) "( )"
    parsedOk (List []) "  (      )  "
    let plus_2_3 = List [Atom "+", Number 2, Number 3]
    parsedOk plus_2_3 "(+ 2 3)"
    parsedOk plus_2_3 "  (+ 2 3)  "
    parsedOk plus_2_3 "( + 2 3)"
    parsedOk plus_2_3 "(+   2     3)"
    parsedOk plus_2_3 "(+ 2 3 )"
    parsedOk plus_2_3 "  (   +   2    3  ) "
    let twice_plus_2_3 = List [Atom "twice", plus_2_3]
    parsedOk twice_plus_2_3 "(twice (+ 2 3))"
    parsedOk twice_plus_2_3 " ( twice   ( +   2  3 ) ) "
    parsedOk (List [Atom "-", Number 5]) "(- 5)"
    

