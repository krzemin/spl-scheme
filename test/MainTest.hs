{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MainTest where

import  Parser
import  Expr
import  Sugar
import  Eval
import  Test.Framework
import  Test.Framework.TestTypes (Assertion)


-- simple framework
evalsTo :: String -> String -> Assertion
evalsTo inTxt outTxt = let
  (Right parsed) = parseScheme inTxt
  (OK _ evalOut) = eval0 $ desugar parsed
  in assertEqual outTxt $ show evalOut

evalsToErr :: String -> String -> Assertion
evalsToErr inTxt outMsg = let
  (Right parsed) = parseScheme inTxt
  (Err errMsg) = eval0 $ desugar parsed
  in assertEqual outMsg errMsg


-- actual tests

test_simpleEvalArith :: Assertion
test_simpleEvalArith = do
  "10" `evalsTo` "10"
  "(+ 10 20)" `evalsTo` "30"
  "(* 2 3)" `evalsTo` "6"
  "(- 8 3)" `evalsTo` "5"
  "(/ 10 5)" `evalsTo` "2"
  "(/ 10 7)" `evalsTo` "1"
  "(% 3 2)" `evalsTo` "1"

test_errorsEvalArith :: Assertion
test_errorsEvalArith = do
  "(/ 5 0)" `evalsToErr` "Division by zero."
  "(% 5 0)" `evalsToErr` "Division by zero."

test_nestedEvalArith :: Assertion
test_nestedEvalArith = do
  "(* (+ 2 (% 11 4)) (- 7 (/ 5 2)))" `evalsTo` "25"
  "(* (+ 2 (% 11 0)) (- 7 (/ 5 2)))" `evalsToErr` "Division by zero."
  "(/ (+ 2 (* 7 8)) (- 7 (+ 5 2)))" `evalsToErr` "Division by zero."

test_simpleEvalLogical :: Assertion
test_simpleEvalLogical = do
  "true" `evalsTo` "#t"
  "false" `evalsTo` "#f"
  "(= 0 0)" `evalsTo` "#t"
  "(= 4 3)" `evalsTo` "#f"
  "(< 1 2)" `evalsTo` "#t"
  "(< 2 2)" `evalsTo` "#f"
  "(< 3 2)" `evalsTo` "#f"
  "(<= 1 2)" `evalsTo` "#t"
  "(<= 2 2)" `evalsTo` "#t"
  "(<= 3 2)" `evalsTo` "#f"
  "(> 1 2)" `evalsTo` "#f"
  "(> 2 2)" `evalsTo` "#f"
  "(> 3 2)" `evalsTo` "#t"
  "(>= 1 2)" `evalsTo` "#f"
  "(>= 2 2)" `evalsTo` "#t"
  "(>= 3 2)" `evalsTo` "#t"
  "(and #t #t)" `evalsTo` "#t"
  "(and #f #t)" `evalsTo` "#f"
  "(and #t #f)" `evalsTo` "#f"
  "(and #f #f)" `evalsTo` "#f"
  "(or #t #t)" `evalsTo` "#t"
  "(or #f #t)" `evalsTo` "#t"
  "(or #t #f)" `evalsTo` "#t"
  "(or #f #f)" `evalsTo` "#f"
  "(not #t)" `evalsTo` "#f"
  "(not #f)" `evalsTo` "#t"

test_nestedEvalLogical :: Assertion
test_nestedEvalLogical = do
  "(< (+ 2 3) (* 2 3))" `evalsTo` "#t"
  "(and (= 3 (+ 1 2)) (or (= 2 3) (not (> 0 0))))" `evalsTo` "#t"
  "(or (<= 8 (+ 0 0)) (and true false))" `evalsTo` "#f"
  "(or (<= 8 (/ 6 0)) (and true false))" `evalsToErr` "Division by zero."

test_equals :: Assertion
test_equals = do
  "(equals? 3 3)" `evalsTo` "#t"
  "(equals? \"abc\" \"abc\")" `evalsTo` "#t"
  "(equals? 2 3)" `evalsTo` "#f"
  "(equals? \"xyz\" \"abc\")" `evalsTo` "#f"
  "(equals? 0 \"abc\")" `evalsTo` "#f"

test_cons :: Assertion
test_cons = do
  "(cons 1 2)" `evalsTo` "'(1 . 2)"
  "(cons (cons 1 2) 3)" `evalsTo` "'((1 . 2) . 3)"
  "(cons 1 (cons 2 3))" `evalsTo` "'(1 2 . 3)"
  "(cons (cons 1 2) (cons 3 4))" `evalsTo` "'((1 . 2) 3 . 4)"
  "(cons (cons 1 2) (cons (cons 3 4) (cons 5 (cons 6 7))))" `evalsTo` "'((1 . 2) (3 . 4) 5 6 . 7)"


  