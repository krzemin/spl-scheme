module Main where

import System.Environment
import System.Console.Readline
import Data.Map hiding (split, map, foldl)
import Data.String.Utils
import Parser
import Eval
import Expr
import Sugar

versionTag :: String
versionTag = "0.1.0.0"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> replMain
    [file] -> interpretFile file
    _ -> showHelp

-- REPL

replMain :: IO ()
replMain = do
  putStrLn $ "Welcome to spl-scheme REPL version " ++ versionTag
  putStrLn "Type in expressions to have them evaluated or :help to get more info."
  repl initEnv

repl :: Env -> IO ()
repl env = do
  maybeLine <- readline "\nspl-scheme> "
  case fmap strip maybeLine of
    Nothing       -> return ()
    Just ":exit"  -> return ()
    Just ":help"  -> printHelp env
    Just ":env"   -> printEnv env
    Just ":reset" -> repl initEnv
    Just line     -> processLine line env

printHelp :: Env -> IO ()
printHelp env = do
  addHistory ":help"
  showHelp
  repl env

showHelp :: IO ()
showHelp = do
  putStrLn "This is mini Scheme-like language implementation written"
  putStrLn "from scratch in Haskell during Semantics of Programming Languages"
  putStrLn "course at CS Institute at University of Wrocław."
  putStrLn ""
  putStrLn "Authors: Piotr Krzemiński, Błażej Saladra"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  spl-scheme       run interactive console (read eval print loop)"
  putStrLn "  spl-scheme file  interpret file"
  putStrLn ""
  putStrLn "Available REPL commands:"
  putStrLn "  :exit            leave the REPL"
  putStrLn "  :help            show this info"
  putStrLn "  :env             show current name bindings"
  putStrLn "  :unbind  name    unbind name from REPL environment"
  putStrLn "  :reset           unbind all names"
  putStrLn "  :desugar expr    show desugared expression"
  putStrLn "  expr             evaluate expression"


printEnv :: Env -> IO ()
printEnv env = do
  addHistory ":env"
  let [m] = env
  mapM_ showSingleBind $ assocs m
  repl env
  where
    showSingleBind (k, v) = putStrLn $ k ++ ": " ++ (show v)

processLine :: String -> Env -> IO ()
processLine line env = do
  addHistoryLine line
  if isUnbindCmd line
    then processUnbind line env
    else if isDesugarCmd line
      then processDesugar line env
      else processEval line env
  where
    addHistoryLine "" = return ()
    addHistoryLine ln = addHistory ln

isUnbindCmd :: String -> Bool
isUnbindCmd = startswith ":unbind"

processUnbind :: String -> Env -> IO ()
processUnbind line env = do
  let [_, x] = split ":unbind" line
  let name = strip x
  repl [delete name $ head env]

isDesugarCmd :: String -> Bool
isDesugarCmd = startswith ":desugar"

processDesugar :: String -> Env -> IO ()
processDesugar line env = do
  let [_, e] = split ":desugar" $ strip line
  case parseScheme e of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right expr -> print $ desugar expr
  repl env

processEval :: String -> Env -> IO ()
processEval line env = do
  case parseScheme line of
    Right expr -> do
      case eval (desugar expr) env of
        OK env' e -> do { print e; repl env' }
        Err s -> do { putStrLn $ "Runtime error: " ++ s; repl env }
        TypeErr s -> do { putStrLn $ "Type error: " ++ s; repl env }
    Left e -> do { putStrLn $ "Parse error: " ++ e; repl env }


-- INTERPRETER

interpretFile :: FilePath -> IO ()
interpretFile file = do
  fileContent <- readFile file
  case parseFileContent fileContent of
    Left e -> putStrLn $ "Parse error" ++ e
    Right exprs -> do
      exprs' <- expandModules exprs
      let exprs'' = map desugar exprs'
      let val = foldl interpretExpr (OK [empty] (List [])) exprs''
      case val of
        OK _ expr -> print expr
        Err e -> putStrLn $ "Runtime error: " ++ e
        TypeErr e -> putStrLn $ "Type error: " ++ e
  return ()

interpretExpr :: Val -> Expr -> Val
interpretExpr (OK env _) expr = eval expr env
interpretExpr v@(Err _) _ = v
interpretExpr v@(TypeErr _) _ = v


expandModules :: [Expr] -> IO [Expr]
expandModules = return



