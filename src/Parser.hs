{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Parser (parseFileContent, parseScheme) where

import           Expr
import           Control.Monad                 (liftM)
import           Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseStr :: Parser Expr
parseStr = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return $ Str x

parseNum :: Parser Expr
parseNum = liftM (Num . read) $ many1 digit

parseAtom :: Parser Expr
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "true" -> Bool True
        "#f" -> Bool False
        "false" -> Bool False
        _    -> Atom atom

parseList :: Parser Expr
parseList = liftM List $ sepBy parseExpr spaces

parseExpr :: Parser Expr
parseExpr = do
    spaces
    y <- parseNum
        <|> parseAtom
        <|> parseStr
        <|> do
            char '('
            spaces
            x <- try parseList
            char ')'
            return x
    spaces
    return y

parseFileContent :: String -> Either String [Expr]
parseFileContent input = case parse (many parseExpr) "" input of
        Left err -> Left (show err)
        Right ast -> Right ast

parseScheme :: String -> Either String Expr
parseScheme "" = Left "empty input"
parseScheme input =
    case parse parseExpr "" input of
        Left err -> Left (show err)
        Right ast -> Right ast
