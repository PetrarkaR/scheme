module Main where
import System.Environment ( getArgs )
import Distribution.TestSuite (TestInstance(name))

main :: IO ()
main = do
    putStrLn ("Name?" )
    name <- getLine
    putStrLn $ "yo name is " ++ name ++ " goofy aah "