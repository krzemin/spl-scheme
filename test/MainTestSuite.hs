{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MainTestSuite where

import Test.Framework

import {-@ HTF_TESTS @-} MainTest

main :: IO ()
main = htfMain htf_importedTests