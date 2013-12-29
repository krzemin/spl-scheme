{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Parser (parseScheme) where

import           Ast
import           Control.Monad                 (liftM)
import           Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser SchemeAst
parseString = do
    char '"'
    x <- many $ noneOf "\""
    char '"'
    return $ String x

parseNumber :: Parser SchemeAst
parseNumber = liftM (Number . read) $ many1 digit

parseAtom :: Parser SchemeAst
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseList :: Parser SchemeAst
parseList = liftM List $ sepBy parseExpr spaces

parseExpr :: Parser SchemeAst
parseExpr = do
    spaces
    y <- parseNumber
        <|> parseAtom
        <|> parseString
        <|> do
            char '('
            spaces
            x <- try parseList
            char ')'
            return x
    spaces
    return y


parseScheme :: String -> Either String SchemeAst
parseScheme "" = Left "empty input"
parseScheme input =
    case parse parseExpr "" input of
        Left err -> Left (show err)
        Right ast -> Right ast
