module ReplMain where

import System.Console.Readline
import Parser
import Eval
import Expr
import Sugar

main :: IO ()
main = do
  putStrLn "Welcome to spl-scheme REPL!"
  repl initEnv
   
repl :: Env -> IO ()
repl env = do
  maybeLine <- readline "> "
  case maybeLine of
    Nothing     -> return ()
    Just "exit" -> return ()
    Just "env" -> do
      addHistory "env"
      print env
      repl env
    Just line   -> do
      addHistory line
      newEnv <- case parseScheme line of
        Right expr -> do
          putStrLn $ "Desugared expr:\n  " ++ show (desugar expr)
          case eval expr env of
            OK env' e -> do
              print e
              return env'
            Err s -> do
              putStrLn $ "Runtime error: " ++ s
              return env
            TypeErr s -> do
              putStrLn $ "Type error: " ++ s
              return env
        Left e -> do
          putStrLn $ "Parse error: " ++ e
          return env
      repl newEnv
