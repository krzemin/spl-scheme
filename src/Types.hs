module Types where

import Expr

data TypeDef repr = TypeDef {
  name :: String,
  toRepr :: Expr -> Maybe repr,
  fromRepr :: repr -> Expr
}

typed :: TypeDef repr -> (repr -> Val) -> Cont
typed t cont _ val =
  case (toRepr t) val of
    Just v -> cont v
    Nothing -> TypeErr $ "Expected type `" ++ name t ++ "`, given " ++ show val


numType :: TypeDef Int
numType = TypeDef "num" extract Num where
  extract (Num n) = Just n
  extract _ = Nothing

boolType :: TypeDef Bool
boolType = TypeDef "bool" extract Bool where
  extract (Bool b) = Just b
  extract _ = Nothing

atomType :: TypeDef String
atomType = TypeDef "atom" extract Atom where
  extract (Atom a) = Just a
  extract _ = Nothing

strType :: TypeDef String
strType = TypeDef "string" extract Atom where
  extract (Str s) = Just s
  extract _ = Nothing

consType :: TypeDef [Expr]
consType = TypeDef "cons" extract List where
  extract (List ls@[Atom "cons", _, _]) = Just ls
  extract _ = Nothing

cloType :: TypeDef (CloFun)
cloType = TypeDef "closure" extract consFun where
  extract (Clo fun) = Just fun
  extract _ = Nothing
  consFun = Clo
