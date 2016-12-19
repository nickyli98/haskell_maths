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

--Tranposes a matrix
transpose :: [[Float]] -> [[Float]]
transpose a
   = transpose' a (length (head a)) 0
    where
      transpose' :: [[Float]] -> Int -> Int -> [[Float]]
      transpose' a x y
       | x == y = []
       | otherwise = [b !! y | b <- a] : transpose' a x (y + 1)
{-
-- Converts a matrix to RREF
rref :: [[Float]] -> [[Float]]
rref a = rref' a b
   where
     b = [findConsecutiveZeros x | x <- a]
     rref' :: [[Float]] -> [Float] -> [[Float]]
     rref' a b
       | ordered b = rowReduce a b
       | otherwise = order a b
-}
--checks if a list os ordered
ordered :: [Float] -> Bool
ordered [] = True
ordered [x] = True
ordered (x:y:xs)
   | x <= y = ordered (y:xs)
   | otherwise = False

-- Swaps rows in a Matrix by using the number of zeros
order :: [[Float]] -> [Float] -> [[Float]]
order a (x:xs)

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
