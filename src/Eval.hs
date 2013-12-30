module Eval where

import  Prelude hiding (lookup)
import  Ast
import  Data.Map hiding (foldl)

builtinFunctions :: Map String ([SchemeAst] -> Either String SchemeAst)
builtinFunctions = fromList
    [
        ("+", arithmeticOperator (+))

    ]
    
arithmeticOperator :: (Int -> Int -> Int) -> [SchemeAst] -> Either String SchemeAst
arithmeticOperator _ [] = Left "Builtin operator arity mismatch (expected at least one argument)"
arithmeticOperator op (a:args) = arithmeticOperator' args a
    where
        arithmeticOperator' [] acc = return acc
        arithmeticOperator' ((Number n):args) (Number acc) = arithmeticOperator' args (Number $ op acc n)
        arithmeticOperator' _ _ = Left "Arithmetic operator expected number"
        
        
        
eval :: SchemeAst -> Either String SchemeAst
eval (Number n) = Right (Number n)
eval (Bool b) = Right (Bool b)
eval (Atom a) = Right (Atom a)
eval (String s) = Right (String s)

eval (List ((Atom function) : arguments)) =  do
    ts <- mapM eval arguments
    case lookup function builtinFunctions of
        Nothing -> Left $ "Function " ++ function ++ " not found"
        Just f -> f ts

eval (List terms) = do
    ts <- mapM eval terms
    return (List ts)








