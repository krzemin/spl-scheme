module Ast where

data SchemeAst = Num Int
               | Atom String
               | Str String
               | List [SchemeAst]
               deriving (Eq)

instance Show SchemeAst where
  show (Num n) = show n
  show (Atom x) = x
  show (Str s) = "\"" ++ s ++ "\""
  show (List []) = "()"
  show (List (x:xs)) = foldl (\acc y -> acc ++ ' ' : show y) ('(' : show x) xs ++ ")" 

