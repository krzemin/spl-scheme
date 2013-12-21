{-# OPTIONS_GHC -F -pgmF htfpp #-}
module AstTest where


import Test.Framework

test_nothing = assertEqual ("0") (show (LeafNumber 0))
