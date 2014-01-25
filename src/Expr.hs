module Expr where

import Data.Map hiding (foldl)

data Expr = Num Int
          | Bool Bool
          | Atom String
          | Str String
          | List [Expr]
          | Clo Env Expr
          deriving (Eq)

type Env = [Map String Expr]

initEnv :: Env
initEnv = [empty]

instance Show Expr where
  show (Num n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Atom x) = x
  show (Str s) = "\"" ++ s ++ "\""
  show (List []) = "()"
  show (List (x:xs)) = foldl (\acc y -> acc ++ ' ' : show y) ('(' : show x) xs ++ ")"
  show (Clo _ _) = "#closure"
