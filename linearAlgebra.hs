import Data.List

-- For float ==
epsilon = 0.00001

-- Moves an element around in a list, type not defined in case needs to be used in other types
move n as = head ts : (hs ++ tail ts)
   where (hs, ts) = splitAt n as

-- Pre: the two matrix are multipliable
matrixProduct :: [[Float]] -> [[Float]] -> [[Float]]
matrixProduct a b
   = [rowOutput x (transpose b) | x <- a]
    where
      rowOutput :: [Float] -> [[Float]] -> [Float]
      rowOutput _ [] = []
      rowOutput a (x:xs)
         = sumOfProducts a x : rowOutput a xs

--Takes two lists of Floats and sums the products in order
--Pre the two lists are of same length
sumOfProducts :: [Float] -> [Float] -> Float
sumOfProducts a b
   = sum (zipWith (*) a b)

-- All pivot columns only has one non zero
-- Assume is already REF
isRREF :: [[Float]] -> Bool
isRREF a = isRREF' (transpose a) 0
   where
     isRREF' :: [[Float]] -> Int -> Bool
     isRREF' [] _ = True
     isRREF' (a:as) x
        | isNonPivot a x = isRREF' as x
        | allZeros (take x a) = isRREF' as (x + 1)
        | otherwise = False

-- Checks if its REF all LHS dia 0s
-- PRE assume its ordered
isREF :: [[Float]] -> Bool
isREF a = isREF' (transpose a) 0
   where
     isREF' :: [[Float]] -> Int -> Bool
     isREF' [] _ = True
     isREF' (a:as) x
      | x >= (length a) - 1 = True
      -- once it reaches final row in matrix it is True
      | (abs(a !! x) < epsilon) = isREF' as x
      -- next column in matrix if its a non-pivot
      | allZeros (drop (x + 1) a) = isREF' as (x + 1)
      | otherwise = False

-- All elements in the list is 0
allZeros :: [Float] -> Bool
allZeros [] = True
allZeros (x:xs)
  | abs(x) < epsilon = allZeros xs
  | otherwise = False

-- Check if list is ordeded small to large
isOrdered :: [Float] -> Bool
isOrdered [] = True
isOrdered [x] = True
isOrdered (x:y:xs)
   | x <= y = isOrdered (y:xs)
   | otherwise = False

-- Checks if column is non pivot
isNonPivot :: [Float] -> Int -> Bool
isNonPivot a x = (abs(a !! x) < epsilon)

-- Converts a matrix to REF (LHS Dia All 0)
ref :: [[Float]] -> [[Float]]
ref a = ref' (order a b) b 0 0
   where
     b = [findConsecutiveZeros x | x <- a]
     ref' :: [[Float]] -> [Int] -> Int -> Int -> [[Float]]
     ref' a b x y
       | isREF a = a
       | empty (drop (x + 1) b) x = ref' a b (x + 1) (y + 1)
       | otherwise = ref' (order ((take (y + 1) a) ++ (ref'' (drop (y + 1) a) x (a !! y))) b) b (x + 1) (y + 1)
       where
         -- Checks if all rows (of same col) below are 0s
         empty :: [Int] -> Int -> Bool
         empty [] _ = True
         empty (b:bs) x
           | b > x = empty bs x
           | otherwise = False
         ref'' :: [[Float]] -> Int -> [Float] -> [[Float]]
         ref'' [] _ _ = []
         ref'' (a:as) x reference
            = rowReduce ((a !! x)/(reference !! x)) a reference : ref'' as x reference

-- Swaps rows in a Matrix by using the number of zeros
order :: [[Float]] -> [Int] -> [[Float]]
order a [] = a
order a x = order' a x (minimum x)
   where
     order' :: [[Float]] -> [Int] -> Int -> [[Float]]
     order' a x b
       = move (findRow x b) a

-- Finds where an element first occurs in a list, works for floats
findRow :: [Int] -> Int -> Int
findRow x y = findRow' x y 0
   where
     findRow' [] _ a = a
     findRow' (x:xs) y a
        | (y - x) == 0 = a
        | otherwise = findRow' xs y (a + 1)

-- Row A - Row B of multiple x
-- Pre the rows are same length
rowReduce :: Float -> [Float] -> [Float] -> [Float]
rowReduce x a b
   = zipWith (-) a [x * bs | bs <- b]

-- Finds the number of consecutive zeros in the beginning on a list of numbers
findConsecutiveZeros :: [Float] -> Int
findConsecutiveZeros a = findConsecutiveZeros' a 0
   where
     findConsecutiveZeros' :: [Float] -> Int -> Int
     findConsecutiveZeros' [] x = x
     findConsecutiveZeros' (x:xs) y
        | x < epsilon = findConsecutiveZeros' xs (y + 1)
        | otherwise = y
