module Evaluator where

import Parser
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


