-- | Main entry point. Print the solution pairs for numbers drawn from [2, n]
-- (n = 100 by default, or the single command line argument).
module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import NumberPuzzle (solutions)

main :: IO ()
main = do args <- getArgs
          case args of
            []    -> run 100
            [arg] -> case readMaybe arg :: Maybe Int of
                       Just n | n >= 2 -> run n
                       _               -> putStrLn "Error: argument must be an integer >= 2"
            _     -> putStrLn "Usage: puzzle [upper-bound]"
  where
    run n = do putStrLn ("Solution pairs for numbers in [2," ++ show n ++ "]: ")
               print (solutions [2..n])
