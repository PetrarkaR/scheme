module Main where

import Parser(readExpr,eval, runRepl, evalAndPrint,runOne)
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


main :: IO ()
main = do 
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    otherwise -> putStrLn "Program only takes 0 or 1  argument"