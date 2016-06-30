module Exercises1010 where

import           Data.Char

-- 1. Given the following sets of consonants and vowels:
--
--    stops  = "pbtdkg"
--    vowels = "aeiou"
--
--    a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.
--
--    b) Modify that function so that it only returns the combinations that begin with a p.
--
--    c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.

stops  = "pbtdkg"
vowels = "aeiou"

-- a)
stopVowelStop :: String -> String -> [(Char, Char, Char)]
stopVowelStop s v = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

-- b)
stopVowelStopPStart :: String -> String -> [(Char, Char, Char)]
stopVowelStopPStart s v = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']

nouns = ["dog", "chair", "flashlight"]
verbs = ["hits", "jumps", "falls", "harpoons"]

-- c)
nounVerbNoun :: [String] -> [String] -> [(String, String, String)]
nounVerbNoun n v = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

-- 2. Average word length from a string of words
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

--    (Implement using fractional division)
avgWordLength :: String -> Double
avgWordLength x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

foldOr :: [Bool] -> Bool
foldOr = foldr (||) False

foldAny :: (a -> Bool) -> [a] -> Bool
foldAny f = foldr (\a b -> f a || b) False

foldElem :: Eq a => a -> [a] -> Bool
foldElem e = foldr (\a b -> a == e || b) False

foldElemUsingAny :: Eq a => a -> [a] -> Bool
foldElemUsingAny = foldAny . (==)

foldReverse :: [a] -> [a]
foldReverse = foldl (flip (:)) []

myFoldMap :: (a -> b) -> [a] -> [b]
myFoldMap f = foldr (\a b -> f a : b) []

myFoldFilter :: (a -> Bool) -> [a] -> [a]
myFoldFilter f = foldr (\a b -> if f a then a : b else b) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a b -> f a ++ b) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\a b -> case f a b of
                                 GT -> a
                                 _ -> b) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\a b -> case f a b of
                                 LT -> a
                                 _ -> b) x xs

