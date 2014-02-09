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



