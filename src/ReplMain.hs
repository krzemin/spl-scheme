module ReplMain where

import Ast

main :: IO ()
main = do
    putStrLn "Hello in spl-scheme REPL"
    print $ List [Atom "+", Number 5, List [Atom "*", Number 3, Number 7], Atom "x"]

