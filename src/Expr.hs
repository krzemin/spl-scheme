module Expr where

import Data.Map hiding (foldl)

data Expr = Num Int
          | Bool Bool
          | Atom String
          | Str String
          | List [Expr]
          | Clo CloFun
type CloFun = Expr -> Cont -> Val

instance Eq Expr where
  Num n1 == Num n2 = n1 == n2
  Bool b1 == Bool b2 = b1 == b2
  Atom s1 == Atom s2 = s1 == s2
  Str s1 == Str s2 = s1 == s2
  List l1 == List l2 = l1 == l2
  _ == _ = False

data Val = OK Env Expr | Err String | TypeErr String
type Cont = Env -> Expr -> Val

type Env = [Map String Expr]

initEnv :: Env
initEnv = [empty]

instance Show Expr where
  show (Num n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Atom x) = x
  show (Str s) = "\"" ++ s ++ "\""
  show cons@(List [Atom "cons", _, _]) = "'(" ++ showCons cons ++ ")"
    where
      showCons (List [Atom "cons", left@(List [Atom "cons", _, _]), right@(List [Atom "cons", _, _])]) =
        "(" ++ showCons left ++ ") " ++ showCons right
      showCons (List [Atom "cons", left@(List [Atom "cons", _, _]), right]) =
        "(" ++ showCons left ++ ") . " ++ showCons right
      showCons (List [Atom "cons", left, right@(List [Atom "cons", _, _])]) =
        showCons left ++ " " ++ showCons right
      showCons (List [Atom "cons", left, right]) =
        showCons left ++ " . " ++ showCons right
      showCons expr = show expr
  show (List []) = "'()"
  show (List (x:xs)) = foldl (\acc y -> acc ++ ' ' : show y) ("'(" ++ show x) xs ++ ")"
  show (Clo _) = "#closure"
 