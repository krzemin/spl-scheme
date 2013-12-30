module ReplMain where

import System.IO
import Parser

main :: IO ()
main = do
    putStrLn "Welcome to spl-scheme REPL!"
    loop
   
loop :: IO ()
loop = do
    putStr "> "
    hFlush stdout
    line <- getLine
    case parseScheme line of
        Right ast -> do
            print ast
            loop
        Left err ->
            putStrLn err
