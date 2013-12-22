module Ast where

data SchemeAst = Number Int
               | Bool Bool
               | Atom String
               | String String
               | List [SchemeAst]
               deriving (Eq)

instance Show SchemeAst where
    show (Number n) = show n
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Atom x) = x
    show (String s) = "\"" ++ s ++ "\""
    show (List []) = "()"
    show (List (x:xs)) = foldl (\acc y -> acc ++ ' ' : show y) ('(' : show x) xs ++ ")"

