-- | Tests for the two puzzle solvers (base only: no test framework needed).
module Main (main) where

import Control.Monad (unless)
import Data.IORef
import Data.List (sort)
import System.Exit (exitFailure)

import AgeProb
import NumberPuzzle

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = unless ok $ do putStrLn ("FAIL: " ++ name); modifyIORef' failures (+ 1)
      equal name got want = check (name ++ " (got " ++ show got ++ ", wanted " ++ show want ++ ")") (got == want)

  -- Age puzzle -----------------------------------------------------------------
  equal "the original puzzle" (solns 13) [[9,2,2]]
  equal "kids 13 are canonical, positive, and sum to 13"
        (all (\k -> length k == 3 && all (>= 1) k && sum k == 13 && k == reverse (sort k)) (kids 13)) True
  equal "kids 6 enumerated" (kids 6) [[2,2,2],[3,2,1],[4,1,1]]
  equal "kids with n < 3 is empty" (map kids [0, 1, 2, -5]) [[], [], [], []]
  equal "prod" (map prod [[9,2,2], [6,6,1], [], [1,1,1]]) [36, 36, 1, 1]
  equal "prodSort is sorted by product" (let ps = map prod (prodSort 13) in ps == sort ps) True
  equal "the ambiguous product 36 groups [9,2,2] with [6,6,1]"
        (filter (\g -> map prod g == [36, 36]) (prodGroup (prodSort 13))) [[[6,6,1],[9,2,2]]]
  equal "oldestSon" (map oldestSon [[9,2,2], [6,6,1], [5,4,4], [1], []]) [True, False, True, False, False]
  equal "sums with a unique answer"   (map solns [14, 26, 28, 29, 34, 39]) [[[8,3,3]], [[18,4,4]], [[16,6,6]], [[15,8,6]], [[20,9,5]], [[27,6,6]]]
  equal "sums with two answers"       (map solns [31, 38]) [[[25,3,3],[14,9,8]], [[27,8,3],[18,10,10]]]
  equal "sums with no answer"         (map solns [1, 2, 3, 12, 15, 20]) [[], [], [], [], [], []]
  equal "every solution has a unique eldest and sums to n"
        (all (\n -> all (\k -> oldestSon k && sum k == n) (solns n)) [1..40]) True

  -- Number puzzle ----------------------------------------------------------------
  let ls = [2..100]
  equal "the original puzzle" (solutions ls) [(4,13)]
  equal "the answer is the same for [2,99]" (solutions [2..99]) [(4,13)]
  equal "no solution for small intervals" (map (\n -> solutions [2..n]) [20, 50]) [[], []]
  equal "sums after Mr. S's statement" (firstValidSums ls (allSums ls)) [11,17,23,27,29,35,37,41,47,53]
  equal "Mr. S does not know either: every candidate sum has several decompositions"
        (all (\s -> length (findPairSums ls s) > 1) (firstValidSums ls (allSums ls))) True
  equal "sums with a single decomposition are never candidates"
        (filter (`elem` firstValidSums ls (allSums ls)) [4, 5, 199, 200]) []
  -- In [2,3,4,6,12] the sum 14 has the single decomposition (2,12), whose product 24 = 2*12 = 4*6
  -- is ambiguous: "I knew you didn't know" holds but "I don't know either" does not.
  equal "a single-decomposition sum with an ambiguous product is rejected"
        (getPossibleSumProds [2, 3, 4, 6, 12] [14]) []
  -- In [2,3,4,6,8] the sum 8 = 2 + 6 = 4 + 4 has two decompositions with ambiguous products
  -- (12 = 2*6 = 3*4, 16 = 2*8 = 4*4): both conditions hold.
  equal "a sum with several decompositions, all with ambiguous products, survives"
        (getPossibleSumProds [2, 3, 4, 6, 8] [8]) [(8, [12, 16])]
  -- In [2,3,4,6] the sum 8 = 2 + 6 = 4 + 4 has a simple product (16 = 4*4 only): rejected.
  equal "a sum with a simple product is rejected"
        (getPossibleSumProds [2, 3, 4, 6] [8, 5]) []
  equal "findPairSums" (findPairSums ls 5) [(2,3)]
  equal "findPairProds" (findPairProds ls 12) [(2,6),(3,4)]
  equal "isSimpleNum" (map (isSimpleNum ls) [4, 6, 12, 10000]) [True, True, False, True]
  equal "unique" (unique [3, 1, 3, 2 :: Int]) [1, 2, 3]
  equal "formSums / formProds count unordered pairs" (length (formSums [2..4]), length (formProds [2..4])) (6, 6)
  equal "allSums / allProds" (allSums [2..4], allProds [2..4]) ([4,5,6,7,8], [4,6,8,9,12,16])
  equal "getSums / getProds" (getSums ls 12, getProds ls 7) ([8, 7], [10, 12])
  equal "the solution is consistent with the dialogue"
        (let ss = allSums ls; fvs = firstValidSums ls ss; fvp = firstValidProds ls ss
             svp = secondValidProds ls fvp fvs; svs = secondValidSums ls fvs svp
         in (17 `elem` fvs, 52 `elem` svp, svs)) (True, True, [17])

  n <- readIORef failures
  if n == 0 then putStrLn "All tests passed." else do putStrLn (show n ++ " test(s) failed."); exitFailure
