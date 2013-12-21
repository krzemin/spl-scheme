{-# OPTIONS_GHC -F -pgmF htfpp #-}
module AstTest where

import Test.Framework
import Ast

assertShown :: String -> SchemeAst -> IO ()
assertShown s x = assertEqual s (show x)

testShowAstNum :: IO ()
testShowAstNum = do
    assertShown "0" (Num 0)
    assertShown "-10" (Num (-10))
    assertShown "2014" (Num 2014)

    
    
--QuickCheck properties

instance Arbitrary SchemeAst where
    arbitrary = do
        n <- choose (-100, 100) :: Gen Int
        return $ Num n

prop_showAst :: SchemeAst -> Bool
prop_showAst (Num n) = show n == show (Num n)
