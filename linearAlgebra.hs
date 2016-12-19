-- Pre: the two matrix are multipliable
matrixProduct :: [[Int]] -> [[Int]] -> [[Int]]
matrixProduct a b
   = [rowOutput x (transpose b) | x <- a]
    where
      rowOutput :: [Int] -> [[Int]] -> [Int]
      rowOutput _ [] = []
      rowOutput a (x:xs)
         = sumOfProducts a x : rowOutput a xs

--Takes two lists of ints and sums the products in order
--Pre the two lists are of same length
sumOfProducts :: [Int] -> [Int] -> Int
sumOfProducts a b
   = sum (zipWith (*) a b)

--Tranposes a matrix
transpose :: [[Int]] -> [[Int]]
transpose a
   = transpose' a (length (head a)) 0
    where
      transpose' :: [[Int]] -> Int -> Int -> [[Int]]
      transpose' a x y
       | x == y = []
       | otherwise = [b !! y | b <- a] : transpose' a x (y + 1)
