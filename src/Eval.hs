module Eval where

import Prelude hiding (lookup)
import Expr
import Types
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
evalTyped t e env = case (toRepr t) e of 
  Just _ -> evalExpr e OK env
  Nothing -> TypeErr $ "Expected type `" ++ name t ++ "`, but given " ++ show e 


evalExpr :: Expr -> Cont -> Env -> Val Expr

evalExpr (Num n) k _ = k $ Num n
evalExpr (Bool b) k _ = k $ Bool b
evalExpr (Str s) k _ = k $ Str s
evalExpr (Atom a) k env = case lookup a env of
  Just e -> evalExpr e k env
  Nothing -> Err $ "Name `" ++ a ++ "` was not bound in current scope."
evalExpr (List ls) k env = evalList ls k env


evalList :: [Expr] -> Cont -> Env -> Val Expr

evalList (Atom "+" : e0 : e1 : []) k env = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  k $ Num (n0 + n1)

evalList (Atom "-" : e0 : e1 : []) k env = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  k $ Num (n0 - n1)

evalList (Atom "*" : e0 : e1 : []) k env = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  k $ Num (n0 + n1)

evalList (Atom "/" : e0 : e1 : []) k env = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  if n1 == 0 then Err "Division by zero." else k $ Num (n0 `div` n1)

evalList (Atom "%" : e0 : e1 : []) k env = do
  v0 <- evalTyped numType e0 env
  let n0 = extractVal numType v0
  v1 <- evalTyped numType e1 env
  let n1 = extractVal numType v1
  if n1 == 0 then Err "Division by zero." else k $ Num (n0 `mod` n1)

evalList (Atom "not" : e : []) k env = do
  v <- evalTyped boolType e env
  let b = extractVal boolType v
  k $ Bool (not b)

evalList (Atom "cond" : b : e0 : e1 : []) k env = do
  v <- evalTyped boolType b env
  let r = extractVal boolType v
  if r then evalExpr e0 k env else evalExpr e1 k env

evalList (Atom "quote" : e : []) k _ = k e

evalList ls _ _ =
  Err $ "Sorry. This is not valid SPL-Scheme expression:\n" ++ show (List ls)


eval :: Expr -> Val Expr
eval e = evalExpr e OK empty
