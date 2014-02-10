module Eval where

import Prelude hiding (lookup)
import Expr
import Types
import Data.Map hiding (foldl)
import Data.Function (fix)

typed :: TypeDef repr -> (repr -> Val) -> Cont
typed t cont _ val =
  case (toRepr t) val of
    Just v -> cont v
    Nothing -> TypeErr $ "Expected type `" ++ name t ++ "`, given " ++ show val

evalExpr :: Expr -> Env -> Cont -> Val

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

numBinOpNoErr :: (Int -> Int -> Int) -> Expr -> Expr -> Env -> Cont -> Val
numBinOpNoErr op e0 e1 env k =
  evalExpr e0 env $ typed numType $ \n0 ->
  evalExpr e1 env $ typed numType $ \n1 ->
  k env $ Num (n0 `op` n1)

numBinOpDivErr :: (Int -> Int -> Int) -> Expr -> Expr -> Env -> Cont -> Val
numBinOpDivErr op e0 e1 env k =
  evalExpr e0 env $ typed numType $ \n0 ->
  evalExpr e1 env $ typed numType $ \n1 ->
  if n1 == 0 then Err "Division by zero." else k env $ Num (n0 `op` n1)

numBoolBinOpNoErr :: (Int -> Int -> Bool) -> Expr -> Expr -> Env -> Cont -> Val
numBoolBinOpNoErr op e0 e1 env k =
  evalExpr e0 env $ typed numType $ \n0 ->
  evalExpr e1 env $ typed numType $ \n1 ->
  k env $ Bool (n0 `op` n1)

boolBinOpNoErr :: (Bool -> Bool -> Bool) -> Expr -> Expr -> Env -> Cont -> Val
boolBinOpNoErr op e0 e1 env k =
  evalExpr e0 env $ typed boolType $ \n0 ->
  evalExpr e1 env $ typed boolType $ \n1 ->
  k env $ Bool (n0 `op` n1)

evalList :: [Expr] -> Env -> Cont -> Val

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

evalList [Atom "not", e] env k =
  evalExpr e env $ typed boolType $ \b ->
  k env $ Bool (not b)

evalList [Atom "equals?", e0, e1] env k =
  evalExpr e0 env $ \_ v0 ->
  evalExpr e1 env $ \_ v1 ->
  k env $ Bool (v0 == v1)

evalList [Atom "cond", b, e0, e1] env k =
  evalExpr b env $ typed boolType $ \bv ->
  if bv then evalExpr e0 env k else evalExpr e1 env k

evalList [Atom "cons", e0, e1] env k =
  evalExpr e0 env $ \_ v0 ->
  evalExpr e1 env $ \_ v1 ->
  k env $ List [Atom "cons", v0, v1]

evalList [Atom "car", e] env k =
  evalExpr e env $ typed consType $ \[Atom "cons", v0, _] ->
  k env $ v0

evalList [Atom "cdr", e] env k =
  evalExpr e env $ typed consType $ \[Atom "cons", _, v1] ->
  k env $ v1

evalList [Atom "quote", e] env k = k env e

evalList [Atom "define", Atom x, e] env@(m:_) k
  | member x m = Err $ "Name " ++ x ++ " was already defined in current scope."
  | otherwise =
      evalExpr e env $ \(m':ms') v ->
      k (insert x v m' : ms') v

evalList (Atom "begin" : e : es) env k = evalBlock (e:es) (empty : env)
  where
    evalBlock [e'] env' =
      evalExpr e' env' $ \(_:ms) v ->
      k ms v
    evalBlock (e':es') env' =
      evalExpr e' env' $ \env'' _ -> evalBlock es' env''
    evalBlock [] _ = undefined -- not valid expression

evalList [Atom "letrec", Atom f, Atom x, e0, e] (m:ms) k =
  evalExpr e (insert f (Clo fun) m : ms) k
  where
    fun = fix (\g v k' -> evalExpr e0 (insert x v (insert f (Clo g) m) : ms) k')

evalList [Atom "lambda", Atom x, e] env@(m:ms) k = k env $ Clo f
  where
    f v k' = evalExpr e (insert x v m : ms) k'

evalList [e0, e1] env k =
  evalExpr e0 env $ typed cloType $ \f ->
  evalExpr e1 env $ \_ v ->
  f v k

evalList (e0 : e1 : es) env k =
  evalList (List [e0, e1] : es) env k

evalList ls _ _ =
  Err $ "Sorry, this is not valid SPL-Scheme expression:\n" ++ show (List ls)


eval :: Expr -> Env -> Val
eval e env = evalExpr e env OK

eval0 :: Expr -> Val
eval0 e = eval e [empty]
