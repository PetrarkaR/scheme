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
import Numeric (readOct, readHex, readInt)


data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | String String
            | Number Integer
            | Bool Bool
spaces :: Parser ()
spaces = skipMany space
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> noneOf "\"\\")
    char '"'
    return $ String x

escapedChar :: Parser Char
escapedChar = do
    char '\\'
    c <- oneOf "\"\\nrt"
    return $ case c of
        '"'  -> '"'   -- escaped quote
        '\\' -> '\\'  -- escaped backslash
        'n'  -> '\n'  -- newline
        'r'  -> '\r'  -- carriage return
        't'  -> '\t'  -- tab
        _    -> c     -- anything else is returned as

parseAtom :: Parser LispVal
parseAtom = do 
            first <- letter <|> symbol 
            rest <- many (letter <|> digit <|> symbol )
            let atom = first:rest
            return $ case atom of 
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseOctalHex :: Parser LispVal
parseOctalHex = do
    char '0'
    t <- oneOf "xo"
    digits <- many1 hexDigit
    let parsedNum = case t of
            'o' ->  readOct digits  :: [(Integer, String)]
            'x' ->  readHex digits  :: [(Integer, String)]
    return $ Number parsedNum
parseDec :: Parser LispVal
parseDec = do
    x <- many1 digit 
    return $ Number $ read x

parseNumber :: Parser LispVal
parseNumber = do
            parseOctalHex <|> parseDec


symbol::Parser Char
symbol =oneOf "!#$%&|*+-/:<=>?@^_~"
parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"


main :: IO ()
main = do
    args <- getArgs
    case args of
        (expr:_) -> putStrLn (readExpr expr)
        _ -> putStrLn "No input provided"
