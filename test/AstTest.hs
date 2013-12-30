{-# OPTIONS_GHC -F -pgmF htfpp #-}
module AstTest where

import Test.Framework
import Test.Framework.TestTypes (Assertion)
import Ast

assertShown :: String -> SchemeAst -> Assertion
assertShown s x = assertEqual s (show x)

test_ShowAstNum :: Assertion
test_ShowAstNum = do
    assertShown "0" (Number 0)
    assertShown "-10" (Number (-10))
    assertShown "2014" (Number 2014)

    
    
--QuickCheck properties

instance Arbitrary SchemeAst where
    arbitrary = do
        n <- choose (-100, 100) :: Gen Int
        return $ Number n

prop_showAst :: SchemeAst -> Bool
prop_showAst (Number n) = show n == show (Number n)
