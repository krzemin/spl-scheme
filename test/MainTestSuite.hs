{-# OPTIONS_GHC -F -pgmF htfpp #-}
module MainTestSuite where

--import System.Environment ( getArgs )
--import System.Exit ( exitWith )
import Test.Framework

import {-@ HTF_TESTS @-} MainTest

main :: IO ()
main = htfMain htf_importedTests