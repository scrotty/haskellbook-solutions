module DbFoldExercises where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 3001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1. Write a function that filters for DbDate values and returns a list of
-- the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getTimes []
  where
    getTimes :: DatabaseItem -> [UTCTime] -> [UTCTime]
    getTimes (DbDate time) dates = time:dates
    getTimes _ dates = dates

-- 2. Write a function that filters for DbNumber values and returns a list
-- of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getDbNumber []
  where
    getDbNumber :: DatabaseItem -> [Integer] -> [Integer]
    getDbNumber (DbNumber n) ns = n:ns
    getDbNumber _ ns = ns

-- 3. Write a function that gets the most recent date
myMaximum :: Ord a => [a] -> a
myMaximum = foldr1 (\x acc -> if x > acc then x else acc)

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = myMaximum . filterDbDate

-- 3. Write a function that sums all the DbNumber values
mySummer :: [Integer] -> Integer
mySummer = foldr (+) 0

sumDb :: [DatabaseItem] -> Integer
sumDb = mySummer . filterDbNumber

-- 3. Write a function that get the average of the DbNumber value
myAverager :: [Integer] -> Double
myAverager nums = fromIntegral (sum nums) / fromIntegral (length nums)

avgDb :: [DatabaseItem] -> Double
avgDb = myAverager . filterDbNumber
