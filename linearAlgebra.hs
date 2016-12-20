import Data.List

-- For float ==
epsilon = 0.00001

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

-- Converts a matrix to RREF
rref :: [[Float]] -> [[Float]]
rref a = rref' (order a b) b
   where
     b = [findConsecutiveZeros x | x <- a]
     rref' :: [[Float]] -> [Float] -> [[Float]]
     rref' a b
       | 

--checks if a list os ordered
ordered :: [Float] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs)
   | x <= y = ordered (y:xs)
   | otherwise = False

-- Swaps rows in a Matrix by using the number of zeros
order :: [[Float]] -> [Float] -> [[Float]]
order a [] = a
order a x = order' a x (minimum x)
   where
     order' :: [[Float]] -> [Float] -> Float -> [[Float]]
     order' a x b
       = move (findRow x b) a

-- Moves an element around in a list, type not defined in case needs to be used in other types
move n as = head ts : (hs ++ tail ts)
   where (hs, ts) = splitAt n as

-- Just a function that finds where an element first occurs in a list, works for floats
findRow :: [Float] -> Float -> Int
findRow x y = findRow' x y 0
   where
     findRow' [] _ a = a
     findRow' (x:xs) y a
        | abs(y - x) < epsilon = a
        | otherwise = findRow' xs y (a + 1)

-- Row A - Row B of multiple x
-- Pre the rows are same length
rowReduce :: Float -> [Float] -> [Float] -> [Float]
rowReduce x a b
   = zipWith (-) a [x * bs | bs <- b]

-- Finds the number of consecutive zeros in the beginning on a list of numbers
findConsecutiveZeros :: [Float] -> Float
findConsecutiveZeros a = findConsecutiveZeros' a 0
   where
     findConsecutiveZeros' :: [Float] -> Float -> Float
     findConsecutiveZeros' [] x = x
     findConsecutiveZeros' (x:xs) y
        | x == 0 = findConsecutiveZeros' xs (y + 1)
        | otherwise = y
