module Types where

import Expr
import Data.Maybe

data TypeDef repr = TypeDef {
  name :: String,
  toRepr :: Expr -> Maybe repr,
  fromRepr :: repr -> Expr
}


extractVal :: TypeDef repr -> Expr -> repr
extractVal t e = fromJust $ (toRepr t) e


numType :: TypeDef Int
numType = TypeDef "num" exFun Num where
  exFun (Num n) = Just n
  exFun _ = Nothing

boolType :: TypeDef Bool
boolType = TypeDef "bool" exFun Bool where
  exFun (Bool b) = Just b
  exFun _ = Nothing

atomType :: TypeDef String
atomType = TypeDef "atom" exFun Atom where
  exFun (Atom a) = Just a
  exFun _ = Nothing

strType :: TypeDef String
strType = TypeDef "string" exFun Atom where
  exFun (Str s) = Just s
  exFun _ = Nothing

consType :: TypeDef [Expr]
consType = TypeDef "cons" exFun List where
  exFun (List ls@[Atom "cons", _, _]) = Just ls
  exFun _ = Nothing

funType :: TypeDef [Expr]
funType = TypeDef "fun" exFun List where
  exFun (List ls@[Atom "lambda", _, _]) = Just ls
  exFun _ = Nothing
  