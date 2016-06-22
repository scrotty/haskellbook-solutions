module Cipher where

import Data.Char

letterShift :: Char -> Int -> Char
letterShift l s = chr (base + ((ord l - base + s) `mod` 26))
  where base = ord 'A'

caesar :: String -> Int -> String
caesar [] _ = []
caesar (x:xs) s = letterShift (toUpper x) s : caesar xs s
