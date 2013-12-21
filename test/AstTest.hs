{-# OPTIONS_GHC -F -pgmF htfpp #-}
module AstTest where

import Test.Framework
import Test.Framework.TestTypes (Assertion)

import Ast

assertShown :: String -> SchemeAst -> Assertion
assertShown s x = assertEqual s (show x)

testShowAstNum :: Assertion
testShowAstNum = do
    assertShown "0" (Num 0)
    assertShown "-10" (Num (-10))
    assertShown "2014" (Num 2014)

