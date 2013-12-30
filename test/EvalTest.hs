{-# OPTIONS_GHC -F -pgmF htfpp #-}
module EvalTest where

import  Ast
import  Eval
import  Test.Framework
import  Test.Framework.TestTypes (Assertion)


test_eval :: Assertion
test_eval = do
    assertEqual (Right $ Number 0) (eval $ Number 0)
    assertEqual (Right $ Number 0) (eval $ List [Atom "+", Number 0])
    assertEqual (Right $ Number 5) (eval $ List [Atom "+", Number 2, Number 3])
    assertEqual (Right $ Number 11) (eval $ List [Atom "+", Number 2, Number 3, Number 6])
    assertEqual (Left _) (eval $ List [Atom "+"])
    
