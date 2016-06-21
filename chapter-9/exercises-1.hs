module Main where

import Data.Char

filterLowercase :: String -> String
filterLowercase = filter isUpper

initialCap :: String -> String
initialCap [] = []
initialCap (x:xs) = toUpper x : xs

allCaps :: String -> String
allCaps [] = []
allCaps (x:xs) = toUpper x : allCaps xs

capHead :: String -> Char
capHead = toUpper . head

main :: IO ()
main = undefined
