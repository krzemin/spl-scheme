module Eval where

import Prelude hiding (lookup)
import Expr
import Types
import Data.Map hiding (foldl)
import Data.Function (fix)

evalTyped :: TypeDef repr -> Expr -> Env -> Val Expr
evalTyped t e env = do
  v <- evalExpr e env OK
  case (toRepr t) v of 
    Just _ -> return v
    Nothing -> TypeErr $ "Expected type `" ++ name t ++ "`, given " ++ show e 


evalExpr :: Expr -> Env -> Cont -> Val Expr

evalExpr (Num n) env k = k env $ Num n
evalExpr (Bool b) env k = k env $ Bool b
evalExpr (Str s) env k = k env $ Str s
evalExpr (Atom x) env k = case lookupEnv env of
  Just v -> k env v
  Nothing -> Err $ "Name `" ++ x ++ "` was not bound in current scope."
  where
    lookupEnv [] = Nothing
    lookupEnv (m:ms) = case lookup x m of
      Just v -> Just v
      Nothing -> lookupEnv ms

evalExpr (List ls) env k = evalList ls env k
evalExpr clo@(Clo _) env k = k env clo

numBinOpNoErr :: (Int -> Int -> Int) -> Expr -> Expr -> Env -> Cont -> Val Expr
numBinOpNoErr op e0 e1 env k = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  k env $ Num (n0 `op` n1)

numBinOpDivErr :: (Int -> Int -> Int) -> Expr -> Expr -> Env -> Cont -> Val Expr
numBinOpDivErr op e0 e1 env k = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  if n1 == 0 then Err "Division by zero." else k env $ Num (n0 `op` n1)

numBoolBinOpNoErr :: (Int -> Int -> Bool) -> Expr -> Expr -> Env -> Cont -> Val Expr
numBoolBinOpNoErr op e0 e1 env k = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  k env $ Bool (n0 `op` n1)

boolBinOpNoErr :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Env -> Cont -> Val Expr
boolBinOpNoErr op e0 e1 env k = do
  v0 <- evalTyped boolType e0 env
  let n0 = extractVal boolType v0
  v1 <- evalTyped boolType e1 env
  let n1 = extractVal boolType v1
  k env $ Bool (n0 `op` n1)

evalList :: [Expr] -> Env -> Cont -> Val Expr

evalList [Atom "+", e0, e1] env k = numBinOpNoErr (+) e0 e1 env k
evalList [Atom "-", e0, e1] env k = numBinOpNoErr (-) e0 e1 env k
evalList [Atom "*", e0, e1] env k = numBinOpNoErr (*) e0 e1 env k
evalList [Atom "/", e0, e1] env k = numBinOpDivErr div e0 e1 env k
evalList [Atom "%", e0, e1] env k = numBinOpDivErr mod e0 e1 env k
evalList [Atom "=", e0, e1] env k = numBoolBinOpNoErr (==) e0 e1 env k
evalList [Atom "<", e0, e1] env k = numBoolBinOpNoErr (<) e0 e1 env k
evalList [Atom "<=", e0, e1] env k = numBoolBinOpNoErr (<=) e0 e1 env k
evalList [Atom ">", e0, e1] env k = numBoolBinOpNoErr (>) e0 e1 env k
evalList [Atom ">=", e0, e1] env k = numBoolBinOpNoErr (>=) e0 e1 env k
evalList [Atom "and", e0, e1] env k = boolBinOpNoErr (&&) e0 e1 env k
evalList [Atom "or", e0, e1] env k = boolBinOpNoErr (||) e0 e1 env k

evalList [Atom "not", e] env k = do
  v <- evalTyped boolType e env
  let b = extractVal boolType v
  k env $ Bool (not b)

evalList [Atom "equals?", e0, e1] env k = do
  v0 <- evalExpr e0 env OK
  v1 <- evalExpr e1 env OK
  k env $ Bool (v0 == v1)

evalList [Atom "cond", b, e0, e1] env k = do
  v <- evalTyped boolType b env
  let r = extractVal boolType v
  if r then evalExpr e0 env k else evalExpr e1 env k

evalList [Atom "cons", e0, e1] env k = do
  v0 <- evalExpr e0 env OK
  v1 <- evalExpr e1 env OK
  k env $ List [Atom "cons", v0, v1]

evalList [Atom "car", e] env k = do
  v <- evalTyped consType e env
  let [Atom "cons", v0, _] = extractVal consType v
  k env $ v0

evalList [Atom "cdr", e] env k = do
  v <- evalTyped consType e env
  let [Atom "cons", _, v1] = extractVal consType v
  k env $ v1

evalList [Atom "quote", e] env k = k env e

evalList [Atom "define", Atom x, e] env@(m:ms) k  = do
  v <- evalExpr e env OK
  if member x m
    then Err $ "Name " ++ x ++ " was already defined in current scope."
    else k (insert x v m : ms) v

evalList (Atom "begin" : e : es) env k = evalBlock (e:es) (empty : env)
  where
    evalBlock [] _ = undefined
    evalBlock [e'] env'@(_:ms) = do
      v <- evalExpr e' env' k
      k ms v
    evalBlock (e':es') env' =
      evalExpr e' env' (\env'' _ -> evalBlock es' env'')

evalList [Atom "letrec", Atom f, Atom x, e0, e] (m:ms) k =
  evalExpr e (insert f (Clo fun) m : ms) k
  where
    fun = fix (\g v k' -> evalExpr e0 (insert x v (insert f (Clo g) m) : ms) k')

evalList [Atom "lambda", Atom x, e] env@(m:ms) k = k env $ Clo f
  where
    f v k' = evalExpr e (insert x v m : ms) k'

evalList [e0, e1] env k = do
  f <- evalTyped cloType e0 env
  let fun = extractVal cloType f
  v <- evalExpr e1 env OK
  fun v k

evalList (e0 : e1 : es) env k =
  evalList (List [e0, e1] : es) env k

evalList ls _ _ =
  Err $ "Sorry, this is not valid SPL-Scheme expression:\n" ++ show (List ls)


eval :: Expr -> Env -> Val Expr
eval e env = evalExpr e env OK
