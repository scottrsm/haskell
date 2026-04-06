{-# LANGUAGE LambdaCase #-}
{- |
           __Problem Statement:__ 

 Mr. X has a pair of integers (a,b), each drawn from the interval [2,100].
 He gives the product, a * b, to a Mr. P and the sum, a + b, to Mr. S. 

 Mr. P and Mr. S have the following dialog:


__Mr. P:__ "I don't know the numbers."

__Mr. S:__ "I knew you didn't know. I don't know either."

__Mr. P:__ "Now I know the numbers."

__Mr. S:__ "Now I know them too."

 __Question:__ What are the numbers a and b?

            __Problem Solution:__ 

 The code below solves this problem. The answer is: (a,b) == (4,13).
-}
module Main 
(addPair, mulPair, formSums, formProds, findPairSums, findPairProds, unique, isSimpleNum, getPossibleSumProds, getSums, getProds, allSums, allProds, 
firstValidSums, firstValidProds, pSumValid, sPrdValid, secondValidProds, solutions, main )

where 

import qualified Data.Set as Set



-- | Add elements of a pair.
addPair :: (Int, Int) -> Int
addPair  = uncurry (+)

-- | Multiply elements of a pair.
mulPair :: (Int, Int) -> Int
mulPair = uncurry (*)

-- | Compute all possible sums of a list of integers, <ls>, without regard to the
-- sum order. That is, in the resulting list of sums, only count one of 
-- (x + y) or (y + x).
formSums :: [Int] -> [Int]
formSums ls =  [ addPair (e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2]

-- | Compute all possible products of a list of integers, <ls>, without regard to the
-- product order. That is, in the resulting list of products, only count one of 
-- (x * y) or (y * x).
formProds :: [Int] -> [Int]
formProds ls =  [ mulPair (e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2]


-- | Find all the ways (discounting order) that <s> can be written as a 
-- sum from elements from <ls>.
findPairSums :: [Int] -> Int -> [(Int, Int)]
findPairSums ls s = [(e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2, s == e1 + e2]

-- | Find all the ways (discounting order) that <p> can be written as a 
-- product from elements of <ls>.
findPairProds :: [Int] -> Int -> [(Int, Int)]
findPairProds ls p = [(e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2, p == e1 * e2]


-- | Return the unique elements of the list <ls> in sorted order.
unique :: Ord a => [a] -> [a]
unique = Set.toAscList . Set.fromList

            
-- | A simple number is one which has a unique factorization with respect 
-- to the list <ls>. That is, there is only one 
-- pair (discounting order) (e1,e2) in <ls>, such that p = e1 * e2.
isSimpleNum :: [Int] -> Int -> Bool
isSimpleNum ls p = case findPairProds ls p of
                     [_] -> True
                     _   -> False
                      
{- |
 Get "possible" sums/product combinations. 

 This means: For each element, sum, in <sums>, return only those that have 

 the property that each pair (x,y) such that x + y == sum

 has the property that all x * y can be written as a product from <ls> 

 in more than one way.

 This function returns a list of the form: [s, [pr]] where s is a sum made from ls + ls and [pr] are 

 all off the possible products that can be made of numbers from ls * ls which sum to s.

 (We need this as Mr S. says that he doesn't know what the pair is. 

 The only way for Mr S. to know the pair is that there be only one one 

 pair of numbers that add to his sum. So we only look at sums that have more 

 than one combination and more than one product.)
 -}
getPossibleSumProds :: [Int] -> [Int] -> [(Int, [Int])]
getPossibleSumProds ls ss =
    -- Get [(s, [pr])]: All the sums with their corresponding products.
    let posProds = 
            map (\s -> (s, map mulPair (findPairSums ls s))) 
                ss
    in
      -- Filter out any [pr] lists of length == 1.
      filter noSimpleNums posProds
          where
            noSimpleNums (_,ps) =
                -- Get all product pairs for each product.
                let pfacs = map (findPairProds ls) ps 
                in
                  -- Return True if all lists have length > 1
                  all (\case (_:_:_) -> True; _ -> False) pfacs


-- | Get all possible sums that correspond to a product p.
getSums :: [Int] -> Int -> [Int]
getSums ls p = map addPair (findPairProds ls p)

-- | Get all possible products that correspond to a sum s.
getProds :: [Int] -> Int -> [Int]
getProds ls s = map mulPair (findPairSums ls s)


-- | Define and solve problem with previous functions.

-- | Get the unique sums formed by adding all pairs from the two lists.
allSums :: [Int] -> [Int]
allSums ls = unique (formSums ls)

-- | Get the unique products formed by multiplying all pairs from the two lists.
allProds :: [Int] -> [Int]
allProds ls = unique (formProds ls)

{- | Get the first cut at valid info. A list of the form:
 
 (s, ps), where s is a valid sum and ps are the possible products.

 Mr S: "I knew you didn't"

 Mr S has a sum which has the property that all possible ways of producing

 it have the property that each of their products can be produced more than one way.

 Get the first cut at the valid sums. These are the potential sums.
-}
firstValidSums :: [Int] -> [Int] -> [Int]
firstValidSums ls ss =
    map fst (getPossibleSumProds ls ss)

-- | Get the first cut at the valid products.
-- These are the potential products.
firstValidProds :: [Int] -> [Int] -> [Int]
firstValidProds ls ss =
    unique $ concatMap snd (getPossibleSumProds ls ss)

-- | A filter, testing that for a given product there is one and only one sum from
-- the list of potential sums (first potential list).
pSumValid :: Int -> [Int] -> Set.Set Int -> Bool
pSumValid prd ls fvSums =
    let prdSums = getSums ls prd in
    case filter (`Set.member` fvSums) prdSums of
      [_] -> True
      _   -> False

{- | Second pass at valid products.                           

 Mr. P: "Now I know the numbers."

 Mr. P has a product for which there is only one way to produce the corresponding sum

 (from the latest list of potential sums).
-}
secondValidProds :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
secondValidProds ls _ss fvProds fvSums =
    let fvSumsSet = Set.fromList fvSums
    in [ p | p <- fvProds, pSumValid p ls fvSumsSet]

    
-- | A filter, testing that for a given sum there is one and only one product from
-- the list of potential products (second potential list).
sPrdValid :: Int -> [Int] -> Set.Set Int -> Bool
sPrdValid s ls svProds = let prds = getProds ls s
                         in
                           case filter (`Set.member` svProds) prds of
                             [_] -> True
                             _   -> False
                                                                                 
{- | Second pass on valid sums.

 Mr. S: "Now I know the numbers."

 Mr. S has a sum for which there is only one way to produce the corresponding product

 (from the latest list of potential products).
-}
secondValidSums :: [Int] -> [Int] -> [Int] -> [Int]
secondValidSums ls fvSums svProds =
    let svProdsSet = Set.fromList svProds
    in [ s | s <- fvSums, sPrdValid s ls svProdsSet]


{- | Find all the pairs (a,b) such that a + b is one of the sums, <sums>, 
 
 and a * b is one of the products, <prds>.

 __Question:__ "What are the numbers a and b?"

 From what we know of potential sums and potential products, we find all possible pairs 

 which have sums in the list of potential sums and products in the list of potential products.
-}
solutions :: [Int] -> [(Int, Int)]
solutions ls =
    let ss   = allSums ls
        fvs  = firstValidSums ls ss
        fvp  = firstValidProds ls ss
        svp  = secondValidProds ls ss fvp fvs
        svs  = secondValidSums ls fvs svp
        svpSet = Set.fromList svp
        sols' s = let sPairs = findPairSums ls s in
                  [ pair | pair <- sPairs, mulPair pair `Set.member` svpSet ]
    in concatMap sols' svs
          
                     

-- | Main entry point. Return the solution pairs.
main :: IO ()
main = do print "Solution pairs: "
          print  (solutions [2..100])


