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
import Numeric (readOct, readHex, readInt, readFloat)
import Data.Complex

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | String String
            | Number Integer
            | Bool Bool
            | Float Float
            | Rational Integer
            | Complex (Complex LispVal)
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
            'o' ->  fst . head $ readOct digits 
            'x' ->  fst . head $ readHex digits 
    return $ Number parsedNum
parseInt :: Parser LispVal
parseInt = do
    x <- many1 digit 
    return $ Number $ read x
parseFloat :: Parser LispVal
parseFloat = do
    left <- many digit
    t <- oneOf ",."
    right <- many1 digit
    let str = left ++ [t] ++ right
    return $ Float (fst . head $ readFloat str)
parseNumber :: Parser LispVal
parseNumber = parseComplex <|>
            parseFloat <|>
            parseOctalHex <|>
            parseInt
parseComplex:: Parser LispVal
parseComplex = do
    sign <- oneOf "+-"
    imag <- parseInt
    imaginary <- oneOf "ij"
    real <- parseInt
    
    return $ Complex (real :+ imag)

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
