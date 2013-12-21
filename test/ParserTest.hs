{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ParserTest where
import Parser

import Test.Framework
import Test.Framework.TestTypes (Assertion)

testSomething :: Assertion
testSomething = assertEqual "1" (show 11)

