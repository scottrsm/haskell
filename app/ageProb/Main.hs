-- | Have the user input the sum of the three sons' ages; this program prints all solutions.
-- That is: ageProb 13
module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import AgeProb (solns)

main :: IO ()
main = do args <- getArgs
          case args of
            [arg] -> case readMaybe arg :: Maybe Int of
                       Just n  -> do putStr "List of solutions: "
                                     print $ solns n
                       Nothing -> putStrLn "Error: argument must be a number"
            _     -> putStrLn "Usage: ageProb sum-of-ages"
