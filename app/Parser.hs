module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.String (String)
import GHC.Integer (Integer)
import Data.Bool (Bool (True, False))
import Text.ParserCombinators.ReadPrec (reset)
import Control.Monad
import Data.Char (digitToInt,isLetter)
import Data.Either (either)
import Numeric (readOct, readHex, readInt, readFloat)
import Data.Complex
import Data.Typeable
import Control.Monad.Except




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
spaces1 = skipMany1 space
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
    left <- many1 digit
    t <- oneOf ",."
    right <- many1 digit
    let str = left ++ [t] ++ right
    return $ Float (fst . head $ readFloat str)
parseNumber :: Parser LispVal
parseNumber = try parseComplex <|>
            try parseFloat <|>
            try parseOctalHex <|>
            try parseInt
parseComplex:: Parser LispVal
parseComplex = do
    real <- parseInt
    spaces
    sign <- oneOf "+-"
    spaces
    imag <- parseInt
    oneOf "ij"
    let imagVal = case sign of
            '+' -> imagPart
            '-' -> imagPart
    return $ Complex ( real :+  imag)
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces1

parseDottedList :: Parser LispVal
parseDottedList = do
    headElems <- endBy parseExpr spaces1   -- each head element followed by ≥1 space
    char '.'
    spaces1                               -- eat the space after the dot
    tailElem <- parseExpr
    return $ DottedList headElems tailElem


parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    spaces
    x <- parseExpr
    return $ List [Atom "quote", x]
listOrDotted :: Parser LispVal
listOrDotted = between
    (char '('  *> spaces)      -- eat '(' then any spaces
    (spaces *> char ')')       -- eat spaces then ')'
    (try parseDottedList <|> parseList)



symbol::Parser Char
symbol =oneOf "!#$%&|*+-/:<=>?@^_~"
parseExpr :: Parser LispVal
parseExpr =  parseAtom
        <|>  parseString
        <|>  parseNumber
        <|> try parseQuoted
        <|> try listOrDotted

readExpr :: String -> LispVal
readExpr input = case parse (spaces >> parseExpr) "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float f) = show f
showVal (Complex c) = show (realPart c) ++ "+" ++ show (imagPart c) ++ "i"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
            ("-", numericBinop (-)),
            ("*", numericBinop (*)),
            ("/", numericBinop div),
            ("mod", numericBinop mod),
            ("quotient", numericBinop quot),
            ("remainder", numericBinop rem),
            ("symbol?", unaryPred  isSymbol),
            ("string?", unaryPred isString),
            ("==",      numericBoolBinop (==)),
            (">",       numericBoolBinop (>)),
            ("<",       numericBoolBinop (<)),
            (">=",      numericBoolBinop (>=)),
            ("<=",      numericBoolBinop (<=))]
numericBinop :: (Integer->Integer->Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

unpackNum (List[n]) = unpackNum n

unaryPred :: (LispVal -> Bool) -> [LispVal] -> LispVal
unaryPred pred [v] = Bool (pred v)
unaryPred _   args = error $ "Expected 1 argument, got " ++ show (length args)


numericBoolBinop :: (Integer->Integer->Bool) ->[LispVal]->LispVal
numericBoolBinop op [x,y] = Bool $ unpackNum x `op` unpackNum y

isEq :: LispVal -> LispVal -> Bool
isEq (Number n1) (Number n2)= n1==n2

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _          = False


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal