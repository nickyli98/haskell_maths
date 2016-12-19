import Data.List

data Ops = Add | Sub | Mul
         deriving (Eq, Show, Ord)

type Info = (Int, [String])

{-
To use - ghci --> output [list of number inputs] (intended output)
Note: BEDMAS not yet implemented.
Read answers as if they're left hand side bracketted.
-}

calc :: Ops -> Int -> Int -> Int
calc Add x y = x + y
calc Sub x y = x - y
calc Mul x y = x * y

genEq :: Ops -> Int -> String
genEq Add x = show x ++ " + "
genEq Sub x = show x ++ " - "
genEq Mul x = show x ++ " * "

listOfOps :: Int -> [[Ops]]
listOfOps 0 = []
listOfOps 1 = loopOps []
listOfOps n
   = listOfOps' n (loopOps []) 2
      where
        listOfOps' n ops n'
           | n' < n =  listOfOps' n (concatMap loopOps ops) (n' + 1)
           | otherwise = concatMap loopOps ops

loopOps :: [Ops] -> [[Ops]]
loopOps ops
   = [(Add : ops), (Sub : ops), (Mul : ops)]

genTotal :: [Int] -> [Ops] -> Int
genTotal [x] _ = x
genTotal (x:y:xs) (op:ops)
   = genTotal' xs ops (calc op x y)
   where
     genTotal' [] [] total = total
     genTotal' (x:xs) (op:ops) total
        = genTotal' xs ops (calc op total x)

genEqStr :: [Int] -> [Ops] -> String
genEqStr n op
   = concat ((zipWith genEq op n) ++ [show (last n)])

genOutputs :: [Int] -> [Info]
genOutputs [] = error "Empty list."
genOutputs n
   = zip (nub total) (genOutputs' (zip total eqStr) (nub total))
    where
      genOutputs' :: [(Int, String)] -> [Int] -> [[String]]
      genOutputs' raw [] = []
      genOutputs' raw (x:xs)
         = [eq | (n, eq) <- raw, n == x] : (genOutputs' raw xs)
      eqStr = [genEqStr n ops | n <- permN, ops <- permOps]
      total = [genTotal n ops | n <- permN, ops <- permOps]
      nubed = nub total
      permN = permutations n
      permOps = listOfOps ((length n)-1)

output :: [Int] -> Int -> [String]
output n x
   = concat [str | (y, str) <- output, y == x]
    where
       output = genOutputs n
