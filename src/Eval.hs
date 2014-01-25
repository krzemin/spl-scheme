module Eval where

import Prelude hiding (lookup)
import Expr
import Types
import Sugar
import Data.Map hiding (foldl)


data Val expr = OK expr | Err String | TypeErr String
type Env = Map String Expr
type Cont = Expr -> Val Expr


instance Monad Val where
  return = OK
  (OK e) >>= cont = cont e
  (Err s) >>= _ = Err s
  (TypeErr s) >>= _ = TypeErr s 


evalTyped :: TypeDef repr -> Expr -> Env -> Val Expr
evalTyped t e env = do
  v <- evalExpr e OK env
  case (toRepr t) v of 
    Just _ -> return v
    Nothing -> TypeErr $ "Expected type `" ++ name t ++ "`, given " ++ show e 


evalExpr :: Expr -> Cont -> Env -> Val Expr

evalExpr (Num n) k _ = k $ Num n
evalExpr (Bool b) k _ = k $ Bool b
evalExpr (Str s) k _ = k $ Str s
evalExpr (Atom a) k env = case lookup a env of
  Just e -> evalExpr e k env
  Nothing -> Err $ "Name `" ++ a ++ "` was not bound in current scope."
evalExpr (List ls) k env = evalList ls k env




numBinOpNoErr :: (Int -> Int -> Int) -> Expr -> Expr -> Cont -> Env -> Val Expr
numBinOpNoErr op e0 e1 k env = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  k $ Num (n0 `op` n1)

numBinOpDivErr :: (Int -> Int -> Int) -> Expr -> Expr -> Cont -> Env -> Val Expr
numBinOpDivErr op e0 e1 k env = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  if n1 == 0 then Err "Division by zero." else k $ Num (n0 `op` n1)

evalList :: [Expr] -> Cont -> Env -> Val Expr

evalList [Atom "+", e0, e1] k env = numBinOpNoErr (+) e0 e1 k env
evalList [Atom "-", e0, e1] k env = numBinOpNoErr (-) e0 e1 k env
evalList [Atom "*", e0, e1] k env = numBinOpNoErr (*) e0 e1 k env
evalList [Atom "/", e0, e1] k env = numBinOpDivErr div e0 e1 k env
evalList [Atom "%", e0, e1] k env = numBinOpDivErr mod e0 e1 k env

evalList [Atom "not", e] k env = do
  v <- evalTyped boolType e env
  let b = extractVal boolType v
  k $ Bool (not b)

evalList [Atom "cond", b, e0, e1] k env = do
  v <- evalTyped boolType b env
  let r = extractVal boolType v
  if r then evalExpr e0 k env else evalExpr e1 k env

evalList [Atom "cons", e0, e1] k env = do
  v0 <- evalExpr e0 OK env
  v1 <- evalExpr e1 OK env
  k $ List [Atom "cons", v0, v1]

evalList [Atom "car", e] k env = do
  v <- evalTyped consType e env
  let [Atom "cons", v0, _] = extractVal consType v
  k $ v0

evalList [Atom "cdr", e] k env = do
  v <- evalTyped consType e env
  let [Atom "cons", _, v1] = extractVal consType v
  k $ v1

evalList ([Atom "quote", e]) k _ = k e

evalList ls _ _ =
  Err $ "Sorry. This is not valid SPL-Scheme expression:\n" ++ show (List ls)


eval :: Expr -> Val Expr
eval e = evalExpr (desugar e) OK empty
