module Ast where


data SchemeAst = LeafNumber Int
               | LeafAtom String
               | LeafString String
               | List [SchemeAst]
               deriving (Eq)

instance Show SchemeAst where
  show (LeafNumber n) = show n
  show (LeafAtom x) = x
  show (LeafString s) = "\"" ++ s ++ "\""
  show (List []) = "()"
  show (List (x:xs)) = (foldl (\acc y -> acc ++ " " ++ show y) ("(" ++ show x) xs) ++ ")" 

