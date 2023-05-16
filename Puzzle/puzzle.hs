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
(addPair, mulPair, formSums, formProds, findPairSums, findPairProds, unique, isSimpleNum, getPossibleSumProds, getSums, getProds, sums, prods, 
firstValidSums, firstValidProds, pSumValid, sPrdValid, secondValidProds, solutions, main )

where 

import Data.List (foldl', group, intersect, sort)



-- | Add elements of a pair.
addPair  = uncurry (+)

-- | Multiply elements of a pair.
mulPair = uncurry (*)

-- | Compute all possible sums of a list of integers, <ls>, without regard to the
-- sum order. That is, in the resulting list of sums, only count one of 
-- (x + y) or (y + x).
formSums ls =  [ addPair (e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2]

-- | Compute all possible products of a list of integers, <ls>, without regard to the
-- product order. That is, in the resulting list of products, only count one of 
-- (x * y) or (y * x).
formProds ls =  [ mulPair (e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2]


-- | Find all the ways (discounting order) that <s> can be written as a 
-- sum from elements from <ls>.
findPairSums ls s = [(e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2, s == e1 + e2]

-- | Find all the ways (discounting order) that <p> can be written as a 
-- product from elements of <ls>.
findPairProds ls p = [(e1,e2) | e1 <- ls, e2 <- ls, e1 <= e2, p == e1 * e2]


-- | Return the unique elements of the list <ls> in sorted order.
unique ls = map head (group $ sort ls)

            
-- | A simple number is one which has a unique factorization with respect 
-- to the list <ls>. That is, there is only one 
-- pair (discounting order) (e1,e2) in <ls>, such that p = e1 * e2.
isSimpleNum ls p = let prods = findPairProds ls p in
                   length prods == 1 
                      
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
getPossibleSumProds ls sums =
    -- Get [(s, [pr])]: All the sums with their corresponding products.
    let posProds = 
            map (\s -> (s, map mulPair (findPairSums ls s))) 
                sums
    in
      -- Filter out any [pr] lists of length == 1.
      filter noSimpleNums posProds
          where
            noSimpleNums (s,ps) =
                -- Get all product pairs for each product.
                let pfacs = map (findPairProds ls) ps 
                in
                  -- Form a list of True/False for each list based on length of list
                  let  bools = map (\l -> length l > 1) pfacs
                  in
                    -- Return True if all list have length > 1
                    foldl' (&&) True bools


-- | Get all possible sums that correspond to a product p.
getSums ls p = map addPair (findPairProds ls p)

-- | Get all possible products that correspond to a sum s.
getProds ls s = map mulPair (findPairSums ls s)


-- | Define and solve problem with previous functions.

-- | Get the unique sums formed by adding all pairs from the two lists.
sums ls = unique (formSums ls)

-- | Get the unique products formed by multiplying all pairs from the two lists.
prods ls = unique (formProds ls)

{- | Get the first cut at valid info. A list of the form:
 
 (s, ps), where s is a valid sum and ps are the possible products.

 Mr S: "I knew you didn't"

 Mr S has a sum which has the property that all possible ways of producing

 it have the property that each of their products can be produced more than one way.

 Get the first cut at the valid sums. These are the potential sums.
-}
firstValidSums ls sums =
    map fst (getPossibleSumProds ls sums)

-- | Get the first cut at the valid products.
-- These are the potential products.
firstValidProds ls sums =
    unique $ concatMap snd (getPossibleSumProds ls sums)

-- | A filter, testing that for a given product there is one and only one sum from
-- the list of potential sums (first potential list).
pSumValid prd ls firstValidSums  =
    let sums = getSums ls prd in
    length (sums `intersect` firstValidSums) == 1

{- | Second pass at valid products.                           

 Mr. P: "Now I now the numbers."

 Mr. P has a product for which there is only one way to produce the corresponding sum

 (from the latest list of potential sums).
-}
secondValidProds ls sums firstValidProds firstValidSums =
    [ p | p <- firstValidProds, pSumValid p ls firstValidSums]

    
-- | A filter, testing that for a given sum there is one and only one product from
-- the list of potential products (second potential list).
sPrdValid sum ls secondValidProds = let prds = getProds ls sum
                                    in
                                      length (prds `intersect` secondValidProds) == 1
                                                                                 
{- | Second pass on valid sums.

 Mr. S: "Now I now the numbers."

 Mr. S has a sum for which there is only one way to produce the corresponding product

 (from the latest list of potential products).
-}
secondValidSums ls firstValidSums secondValidProds =
    [ s | s <- firstValidSums, sPrdValid s ls secondValidProds]


{- | Find all the pairs (a,b) such that a + b is one of the sums, <sums>, 
 
 and a * b is one of the products, <prds>.

 __Question:__ "What are the numbers a and b?"

 From what we know of potential sums and potential products, we find all possible pairs 

 which have sums in the list of potential sums and products in the list of potential products.
-}
solutions ls =
    let ss = sums ls in
    
    -- Get the first potential lists of sums and products.
    let fvs = firstValidSums ls ss
        fvp = firstValidProds ls ss in
    
    -- The potential products that Mr P might have.
    let svp = secondValidProds ls ss fvp fvs in
    
    -- The potential sums that Mr S might have.
    let svs = secondValidSums ls fvs svp in
    
    -- Solution for a potential sum, find all pairs whose products
    -- are in the list of potential products (svp).
    let sols' s = let sPairs = findPairSums ls s in
                  [ pair | pair <- sPairs, mulPair pair `elem` svp ] in
    
    -- Collect all of the solutions from svs -- the potential sums.
    concatMap sols' svs
          
                     

-- | Main entry point. Return the solution pairs.
main = do print "Solution pairs: "
          print  (solutions [2..100])


