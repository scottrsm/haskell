{-# LANGUAGE LambdaCase #-}
{- |
__A module that solves the Age Problem Puzzle__
  
Mr. A tells a Mr. B that he has three sons. Mr. A then tells Mr. B that he will
give him facts about his children until he has enough information to determine
their ages. Mr. A tells Mr. B to stop him when Mr. B has enough information.
  
__Mr. A to Mr. B:__ "The sum of my boys' ages is 13."

__Mr. A to Mr. B:__ "The product of their ages is your age."

__Mr. A to Mr. B:__ "My eldest son lives in the attic."

__Mr. B:__ "Stop, I know the ages of your sons."
 
__Question:__ What are the ages of the three sons?
 
__Problem Solution:__ 
The code solves this problem more generically via the command line:  ageProb <sum-of-ages>
The solution to the original problem is [9,2,2].
-}
module Main
 (
  Children, main, kids, prod, prodSort, prodGroup, ambiguousProds, oldestSon, potentialSolns, solns
 )

where

import System.Environment  (getArgs)
import Data.List (foldl', groupBy, sortBy)
import Text.Read (readMaybe)

-- | Type 'Children' is a list of ages.
type Children = [Int]

-- | List all possible children in canonical order (oldest to youngest) which sum to n.
-- These are lists of length 3 that are integers. 
-- Note: There are cases where some children may be the same age.
kids :: Int -> [Children]
kids n = [ [x,y,z] | x <- [1..n], y <- [1..x], z <- [1..y], x + y + z == n]

-- | Take a list of children and return the product of their ages.
prod :: Children -> Int
prod = foldl' (*) 1
 
-- | Produce a sorted list of all possible children whose ages sum to <n>.
prodSort :: Int -> [Children]
prodSort n = sortBy (\x y -> compare (prod x) (prod y)) (kids n)

-- | Group lists of children by equal age products.
-- Assumes that the input list is sorted with <prodSort>.
prodGroup :: [Children] -> [[Children]]
prodGroup = groupBy (\x y -> prod x == prod y) 

-- | Filter for lists of length > 1
-- These are the ambiguous lists; otherwise 
-- Mr. B would know the answer and since he didn't stop Mr. A 
-- from giving more information, he needed more info
-- to determine the answer.
ambiguousProds :: [[Children]] -> [[Children]]
ambiguousProds = filter (\case (_:_:_) -> True; _ -> False) 

-- | Check a child list for an eldest son.
oldestSon :: Children -> Bool
oldestSon (a:b:_) = a /= b
oldestSon _        = False

-- | Get Potential Solutions.
-- Strategy: Get all potential solutions based on the following criterion:
--
-- 0. Represent the list of all potential children in a canonical representation.
--
-- 1. Group the potential children by common age product.
--
-- 2. Filter for groups of length > 1 (Mr. B needed more info)
--
-- 3. Within the groups, filter out potential solutions that
--    don't have an eldest son.
potentialSolns :: Int -> [[Children]]
potentialSolns n = map (filter oldestSon) $ ambiguousProds (prodGroup (prodSort n))

-- | Get Solutions:
-- Filter potential solutions to get final solutions.
-- From the list of potential solutions, filter for
-- singleton lists -- as these are the ones which allow
-- Mr. B to know the answer -- and then concatenate them
-- together to get all possible solutions.
solns :: Int -> [Children]
solns n = concat $ filter (\case [_] -> True; _ -> False) (potentialSolns n)

          
-- | Have user input the sum of the three sons ages, this program will provide all solutions.
-- That is: program_name 13
main :: IO ()
main = do args <- getArgs
          case args of
            [arg] -> case readMaybe arg :: Maybe Int of
                       Just n  -> do putStr "List of solutions: "
                                     print $ solns n
                       Nothing -> putStrLn "Error: argument must be a number"
            _     -> putStrLn "Usage: ageProb sum-of-ages"

            
