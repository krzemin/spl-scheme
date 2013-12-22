{-# OPTIONS_GHC -F -pgmF htfpp #-}
module AstTestSuite where

import Test.Framework

import {-@ HTF_TESTS @-} AstTest
import {-@ HTF_TESTS @-} ParserTest

main :: IO()
main = htfMain htf_importedTests
