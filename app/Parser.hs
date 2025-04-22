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
import System.IO
import Data.IORef
import Control.Monad.IO.Class(liftIO)

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IO LispVal
getVar envRef var = do
  env <- readIORef envRef
  case lookup var env of
    Just valueRef -> readIORef valueRef
    Nothing       -> error $ "Unbound variable: " ++ var

setVar :: Env -> String -> LispVal -> IO LispVal
setVar envRef var value = do
  env <- readIORef envRef
  case lookup var env of
    Just valueRef -> do
      writeIORef valueRef value
      return value
    Nothing -> do
      return $ String "Unbound variable" 

defineVar :: Env -> String -> LispVal ->IO LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef):env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)


extractValue :: LispVal-> LispVal
extractValue val = val


flushStr :: String -> IO ()
flushStr str=putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = do
  let parsedExpr = readExpr expr
  result <- eval env parsedExpr  -- Note the <- instead of let
  return $ show result

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

runOne ::String->IO()
runOne expr = primitiveBindings >>= flip evalAndPrint expr


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action
runRepl :: IO()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "input>>> ") . evalAndPrint

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

--makeNormalFunc = makeFunc Nothing
makeNormalFunc env params body = makeFunc Nothing env params body
--makeVarArgs = makeFunc . Just . showVal
makeVarArgs argName env params body = makeFunc (Just argName) env params body




data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | String String
            | Number Integer
            | Bool Bool
            | Float Float
            | Rational Integer
            | Complex (Complex LispVal)
            | PrimitiveFunc ([LispVal] -> LispVal)
            | Func { params ::[ String], vararg::(Maybe String),
                    body:: [LispVal],closure ::Env}





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

showVal(PrimitiveFunc _) = "<primitive>"
showVal(Func {params = args, vararg=varargs, body=body, closure =env})=
    "(lambda (" ++unwords (map show args) ++ (
        case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg ++ ") ... )"
    )
getAtomName :: LispVal -> String
getAtomName (Atom name) = name
getAtomName other = error $ "Expected Atom, got: " ++ show other

eval :: Env -> LispVal -> IO LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (List (Atom "define" : List (Atom fname : params) : body)) = do
  -- Create a function that takes the given params and evaluates the body
  makeNormalFunc env params body >>= defineVar env fname
eval env (List [Atom "quote", val]) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "if", pred, conseq, alt]) = do
  predResult <- eval env pred
  case predResult of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    _ -> error "Predicate in 'if' must evaluate to a boolean"
eval env (List [Atom "set!", Atom var, form]) = do
  result <- eval env form
  setVar env var result
eval env (List [Atom "define", Atom var, form]) = do
  result <- eval env form
  defineVar env var result
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals



eval env (List (Atom "lambda" : DottedList params vararg : body)) =
    case vararg of
        Atom name -> makeVarArgs name env params body
        _         -> error "Expected atom as vararg name"


eval env (List (Atom "lambda" : vararg@(Atom _) : body)) =
    case vararg of
        Atom name -> makeVarArgs name env [] body
        _         -> error "Expected atom as vararg name"


eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    --makeVarArgs varargs env params body
    case varargs of 
        Atom name -> makeVarArgs name env params body
        _         -> error "oops"
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    case varargs of
    Atom name -> makeVarArgs name env [] body
    _         -> error "Invalid varargs in lambda shorthand"

--apply :: String -> [LispVal] -> LispVal
apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc func) args = return $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then error "Wrong number of arguments"
        else (liftIO $ bindVars closure $ zip params args) >>=
            bindVarArgs varargs >>= evalBody
  where 
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing -> return env
apply val _ = error $ "Tried to apply non-function: " ++ show val


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>=( flip bindVars $ map makePrimitiveFunc primitives)
    where makePrimitiveFunc (var,func) = (var,PrimitiveFunc func)

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