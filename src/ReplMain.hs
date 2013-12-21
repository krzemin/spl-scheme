module ReplMain where

import Ast

main :: IO ()
main = do
    putStrLn "Hello in spl-scheme REPL"
    print $ List [Atom "+", Num 5, List [Atom "*", Num 3, Num 7], Atom "x"]

