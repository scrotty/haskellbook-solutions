module MyStandards where

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs)
  | x         = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x       = True
  | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | e == x = True
  | otherwise = myElem e xs

myElemUsingAny :: Eq a => a -> [a] -> Bool
myElemUsingAny = myAny . (==)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myOrderingBy :: (a -> a -> Ordering) -> Ordering -> [a] -> a
myOrderingBy _ _ [x] = x
myOrderingBy f o (x:y:zs) = myOrderingBy f o (h:zs)
  where h = if f x y == o then x else y

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = myOrderingBy f GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = myOrderingBy f LT

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

