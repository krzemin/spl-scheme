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

evalsToTypeErr :: String -> String -> Assertion
evalsToTypeErr inTxt outMsg = let
  (Right parsed) = parseScheme inTxt
  (TypeErr errMsg) = eval0 $ desugar parsed
  in assertEqual outMsg errMsg


-- actual tests

test_simple_eval_arith :: Assertion
test_simple_eval_arith = do
  "10" `evalsTo` "10"
  "(+ 10 20)" `evalsTo` "30"
  "(* 2 3)" `evalsTo` "6"
  "(- 8 3)" `evalsTo` "5"
  "(/ 10 5)" `evalsTo` "2"
  "(/ 10 7)" `evalsTo` "1"
  "(% 3 2)" `evalsTo` "1"

test_errors_eval_arith :: Assertion
test_errors_eval_arith = do
  "(/ 5 0)" `evalsToErr` "Division by zero."
  "(% 5 0)" `evalsToErr` "Division by zero."

test_nested_eval_arith :: Assertion
test_nested_eval_arith = do
  "(* (+ 2 (% 11 4)) (- 7 (/ 5 2)))" `evalsTo` "25"
  "(* (+ 2 (% 11 0)) (- 7 (/ 5 2)))" `evalsToErr` "Division by zero."
  "(/ (+ 2 (* 7 8)) (- 7 (+ 5 2)))" `evalsToErr` "Division by zero."

test_simple_eval_logical :: Assertion
test_simple_eval_logical = do
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

test_nested_eval_logical :: Assertion
test_nested_eval_logical = do
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

test_cond :: Assertion
test_cond = do
  "(cond #t 1 2)" `evalsTo` "1"
  "(cond #f 1 2)" `evalsTo` "2"
  "(cond (= 2 (+ 1 1)) \"abc\" 7)" `evalsTo` "\"abc\""

test_if :: Assertion
test_if = do
  "(if #t 1 2)" `evalsTo` "1"
  "(if #f 1 2)" `evalsTo` "2"
  "(if (= 2 (+ 1 1)) \"abc\" 7)" `evalsTo` "\"abc\""


test_cons :: Assertion
test_cons = do
  "(cons 1 2)" `evalsTo` "'(1 . 2)"
  "(cons (cons 1 2) 3)" `evalsTo` "'((1 . 2) . 3)"
  "(cons 1 (cons 2 3))" `evalsTo` "'(1 2 . 3)"
  "(cons (cons 1 2) (cons 3 4))" `evalsTo` "'((1 . 2) 3 . 4)"
  "(cons (cons 1 2) (cons (cons 3 4) (cons 5 (cons 6 7))))" `evalsTo` "'((1 . 2) (3 . 4) 5 6 . 7)"

test_car_cdr :: Assertion
test_car_cdr = do
  "(car (cons 1 2))" `evalsTo` "1"
  "(cdr (cons 1 2))" `evalsTo` "2"
  "(car (cons (cons 0 1) 2))" `evalsTo` "'(0 . 1)"
  "(cdr (cons 1 (cons 2 3)))" `evalsTo` "'(2 . 3)"

test_fst_snd :: Assertion
test_fst_snd = do
  "(fst (cons 1 2))" `evalsTo` "1"
  "(snd (cons 1 2))" `evalsTo` "2"
  "(fst (cons (cons 0 1) 2))" `evalsTo` "'(0 . 1)"
  "(snd (cons 1 (cons 2 3)))" `evalsTo` "'(2 . 3)"

test_head_tail :: Assertion
test_head_tail = do
  "(head (cons 1 2))" `evalsTo` "1"
  "(tail (cons 1 2))" `evalsTo` "2"
  "(head (cons (cons 0 1) 2))" `evalsTo` "'(0 . 1)"
  "(tail (cons 1 (cons 2 3)))" `evalsTo` "'(2 . 3)"

test_quote :: Assertion
test_quote = do
  "(quote ())" `evalsTo` "'()"
  "(quote (1 2 3))" `evalsTo` "'(1 2 3)"
  "(quote (+ 2 3))" `evalsTo` "'(+ 2 3)"
  "(quote (x y z))" `evalsTo` "'(x y z)"
  "(quote (0 (cons 20 30) 0))" `evalsTo` "'(0 '(20 . 30) 0)"

test_nil :: Assertion
test_nil = do
  "(nil? (cons 1 2))" `evalsTo` "#f"
  "(nil? (quote ()))" `evalsTo` "#t"
  "(nil? nil)" `evalsTo` "#t"
  "(cons 1 nil)" `evalsTo` "'(1 . '())"

test_blocks_define :: Assertion
test_blocks_define = do
  "(begin (+ 2 3))" `evalsTo` "5"
  "(begin (define x 10))" `evalsTo` "10"
  "(begin (define x 10) (+ x 3))" `evalsTo` "13"
  "(begin (define x 10) (define y (+ x 30)) (+ x y))" `evalsTo` "50"
  "(begin (define x 1) (define x 2))" `evalsToErr` "Name x was already defined in current scope."

test_lambda :: Assertion
test_lambda = do
  "(lambda x x)" `evalsTo` "#closure"
  "(lambda (x y z) 10)" `evalsTo` "#closure"
  "(lambda x (lambda y (+ x y)))" `evalsTo` "#closure"
  "(lambda x (/ 10 0))" `evalsTo` "#closure"

test_application :: Assertion
test_application = do
  "((lambda x x) 5)" `evalsTo` "5"
  "((lambda x (+ x 2)) 5)" `evalsTo` "7"
  "((((lambda (x y z) (+ x (+ y z))) 1) 2) 3)" `evalsTo` "6"
  "((lambda (x y z) (+ x (+ y z))) 1 2 3)" `evalsTo` "6"
  "((lambda x (/ 10 x)) 0)" `evalsToErr` "Division by zero."

test_let :: Assertion
test_let = do
  "(let x 5 x)" `evalsTo` "5"
  "(let x (+ 2 2) (+ x x))" `evalsTo` "8"
  "(let* ((x 2) (y 3)) (+ x y))" `evalsTo` "5"
  "(let* ((x 2) (y (+ 2 x))) (+ x y))" `evalsTo` "6"

test_letrec :: Assertion
test_letrec = do
  "(letrec f n (cond (= 0 n) 1 (* n (f (- n 1)))) (f 5))" `evalsTo` "120"
  "(letrec f n (cond (< n 2) n (+ (f (- n 1)) (f (- n 2)))) (f 8))" `evalsTo` "21"

test_letrec_poly :: Assertion
test_letrec_poly = do
  "(letrec* () 0)" `evalsToErr` "Bad letrec* bindings."
  "(letrec* ((f f 0)) (f 1))" `evalsToErr` "Bad letrec* bindings."
  "(letrec* ((f x (if (= 0 x) 1 (* x (f (- x 1)))))) (f 6))" `evalsTo` "720"
  let oddeven prog = "(letrec* (\
    \(odd x (if (= x 0) #f (even (- x 1))))\
    \(even x (if (= x 0) #t (odd (- x 1))))\
  \) " ++ prog ++ ")"
  oddeven "(even 6)" `evalsTo` "#t"
  oddeven "(even 5)" `evalsTo` "#f"
  oddeven "(odd 6)" `evalsTo` "#f"
  oddeven "(odd 5)" `evalsTo` "#t"

test_defun :: Assertion
test_defun = do
  "(begin (defun (f x) (+ x 2)) (f 10))" `evalsTo` "12"
  "(begin (defun (f a b c d) (+ (+ a b) (+ c d))) (f 3 7 9 1))" `evalsTo` "20"

test_type_errors :: Assertion
test_type_errors = do
  "(+ 2 \"abc\")" `evalsToTypeErr` "Expected type `num`, given \"abc\""
  "(5 6)" `evalsToTypeErr` "Expected type `closure`, given 5"
  "(and #f 3)" `evalsToTypeErr` "Expected type `bool`, given 3"
  "(car 10)" `evalsToTypeErr` "Expected type `cons`, given 10"
  "(cdr 10)" `evalsToTypeErr` "Expected type `cons`, given 10"

test_callcc :: Assertion
test_callcc = do
  "(call/cc (lambda k (+ 2 (throw k (* 3 4)))))" `evalsTo` "12"
