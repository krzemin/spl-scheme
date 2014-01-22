module Eval where

import  Prelude hiding (lookup)
import  Expr
import  Data.Map hiding (foldl)

data Val = OK Expr | Err String | TypeErr String
type Env = Map String Expr
type Cont = Expr -> Val
type TypedCont repr = repr -> Val

evalExpr :: Expr -> Cont -> Env -> Val
evalExpr (Num n) k _ = k $ Num n
evalExpr (Bool b) k _ = k $ Bool b
evalExpr (Str s) k _ = k $ Str s
evalExpr (Atom a) k env = case lookup a env of
  Just e -> evalExpr e k env
  Nothing -> Err $ "Name `" ++ a ++ "` was not bound in current scope."
evalExpr (List ls) k env = evalList ls k env

evalList :: [Expr] -> Cont -> Env -> Val
evalList (Atom "+" : e0 : e1 : []) k env = evalExpr e0 k' env where
  k' = typedCont numType (\n0 -> evalExpr e1 (k'' n0) env)
  k'' n0 = typedCont numType (\n1 -> k $ Num (n0 + n1))

evalTyped :: TypeDef repr -> Expr -> TypedCont a -> Env -> Val
evalTyped t e k env = let reprE = (toRepr t) e in k reprE

--evalList (Atom "-" : e0 : e1 : []) k env = do
--  n0 <- evalTyped numType e0 env
--  n1 <- evalTyped numType e1 env
--  k $ Num $ n0 + n1


--evalList (Atom "*" : e0 : e1 : []) k env =
--  evalTyped numType e0 env >>= (\n0 ->
--    evalTyped numType e1 env >>= (\n1 ->
--      k $ Num $ n0 + n1
--    )
--  )




--instance Monad TypedCont where
--  return :: repr -> TypedCont repr
--  return v = OK $ fromRepr v
--  >>= :: TypedCont reprA -> (reprA -> TypedCont reprB) -> TypedCont reprB
--  f >>= g = g f 


--evalList (Atom "not" : e : []) k env = evalExpr e k' env where
--  k' = typedCont boolType (\v -> k $ Bool $ not v)


--evalList ls _ _ = Err $ "Sorry. This is not valid SPL-Scheme expression:\n" ++ show (List ls)


data TypeDef repr = TypeDef {
  name :: String,
  toRepr :: Expr -> Maybe repr,
  fromRepr :: repr -> Expr
} 

typedCont :: TypeDef repr -> TypedCont repr -> Expr -> Val
typedCont t k e = case (toRepr t) e of
  Just v -> k v
  Nothing -> TypeErr $ "Expected type `" ++ name t ++ "`, but given " ++ show e 


numType :: TypeDef Int
numType = TypeDef "int" exFun Num where
  exFun (Num n) = Just n
  exFun _ = Nothing

boolType :: TypeDef Bool
boolType = TypeDef "bool" exFun Bool where
  exFun (Bool b) = Just b
  exFun _ = Nothing

--consType :: TypeDef
--consType = TypeDef "cons" exFun where
--  ckFun (List [Atom "cons", _, _]) = True
--  ckFun _ = False

--funType :: TypeDef
--funType = TypeDef "fun" ckFun where
--  ckFun (List [Atom "lambda", _, _]) = True
--  ckFun _ = False






eval :: Expr -> Val
eval e = evalExpr e OK empty
