-- Pre: the two matrix are multipliable
{-matrixProduct :: [[Int]] -> [[Int]] -> [[Int]]
matrixProduct a b
   = [rowOutput ]
    where
      k = length a
      n = length b
      m = length (head b)-}

transpose :: [[Int]] -> [[Int]]
transpose a
   = tranpose' a (length (head a)) 0

transpose' :: [[Int]] -> Int -> Int -> [[Int]]
tranpose' a x y
   | x == y = []
   | otherwise = [b !! y | b <- a] : tranpose a x (y + 1)
