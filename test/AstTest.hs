{-# OPTIONS_GHC -F -pgmF htfpp #-}
module AstTest where

import Test.Framework
import Ast

test_nothing = assertEqual "0" (show (Num 0))
