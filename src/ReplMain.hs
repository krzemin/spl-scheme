module ReplMain where

import Parser
import Eval
import System.Console.Readline

main :: IO ()
main = do
  putStrLn "Welcome to spl-scheme REPL!"
  repl
   
repl :: IO ()
repl = do
  maybeLine <- readline "> "
  case maybeLine of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line   -> do
      addHistory line
      case parseScheme line of
        Right expr -> do
          case eval expr of
            OK e -> print e
            Err s -> putStrLn $ "Runtime error: " ++ s
            TypeErr s -> putStrLn $ "Type error: " ++ s
        Left e ->
          putStrLn $ "Parse error: " ++ e
      repl
