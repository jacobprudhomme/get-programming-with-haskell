#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

aList :: [[Char]]
aList = ["cat", "dog", "mouse"]

-- Division is not defined in the Num typeclass
-- because (/) does not work for all Num types

addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2

class Describable a where
  describe :: a -> String

smallestInt = minBound :: Int
largestInt = maxBound :: Int
smallestChar = minBound :: Char
largestChar = maxBound :: Char

data IceCream = Chocolate | Vanilla

data IceCream' = Chocolate' | Vanilla' deriving (Eq, Ord, Show)
-- Chocolate' < Vanilla' because it comes first

-- Word seems to be a type for 64-bit binary words
-- It's bounded between 0 and 2^64 - 1 (found using :info)
-- Int is bounded between -2^63 and 2^63 - 1

inc :: Int -> Int
inc x = x + 1
-- The difference between inc and succ (in Enum Int)
-- is that succ will not increment past the maxBound
-- of Int, whereas inc will overflow

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n =
  if n == maxBound -- (==) comes from Eq, maxBound comes from Bounded
  then minBound    -- minBound comes from Bounded
  else succ n      -- succ comes from Enum
