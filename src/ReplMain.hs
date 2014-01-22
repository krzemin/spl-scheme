module ReplMain where

import System.IO
import Parser
import Eval

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
    Right expr -> do
      print expr
      case eval expr of
        OK e -> print e
        Err s -> putStrLn s
        TypeErr s -> putStrLn s
      loop
    Left err ->
      putStrLn err
