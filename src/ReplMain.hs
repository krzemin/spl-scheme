module ReplMain where

import Ast

main :: IO ()
main = do
    putStrLn "Hello in spl-scheme REPL"
    let x = List [LeafAtom "+", LeafNumber 5, List [LeafAtom "*", LeafNumber 3, LeafNumber 7], LeafAtom "x"]
    putStrLn $ show x

