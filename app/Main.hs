module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.String (String)
import GHC.Integer (Integer)
import Data.Bool (Bool (True, False))
import Text.ParserCombinators.ReadPrec (reset)
import Control.Monad
import Data.Char (digitToInt)
import Data.Either (either)

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | String String
            | Number Integer
            | Bool Bool

parseString:: Parser LispVal
parseString = do 
            char '"'
            x<- many (noneOf "\"\\" <|> (string "\\\"" >> return '"'))
            char '"'
            return $ String x

parseAtom:: Parser LispVal
parseAtom = do
            first <- letter <|> symbol
            rest <- many(letter <|> symbol <|> digit)
            let atom =first:rest
            return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom
parseNumber :: Parser LispVal
parseNumber = do
            x <- many1 digit
            return $ Number $ read x
--parseNumber = liftM (Number . read) $ many1 digit
spaces :: Parser ()
spaces = skipMany1 space
symbol::Parser Char
symbol =oneOf "!#$%&|*+-/:<=>?@^_~"
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
