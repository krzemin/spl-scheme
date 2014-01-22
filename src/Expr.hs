module Expr where

data Expr = Num Int
          | Bool Bool
          | Atom String
          | Str String
          | List [Expr]
          deriving (Eq)

instance Show Expr where
  show (Num n) = show n
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Atom x) = x
  show (Str s) = "\"" ++ s ++ "\""
  show (List []) = "()"
  show (List (x:xs)) = foldl (\acc y -> acc ++ ' ' : show y) ('(' : show x) xs ++ ")"

