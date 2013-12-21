{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ParserTest where
import Parser

import Test.Framework

testSomething :: IO ()
testSomething = assertEqual "1" (show 11)

