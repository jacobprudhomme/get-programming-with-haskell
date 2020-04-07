#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import Control.Monad
import Data.Char


data Grade = F | D | C | B | A deriving (Enum, Eq, Ord, Read, Show)

data Degree = HS | BA | MS | PhD deriving (Enum, Eq, Ord, Read, Show)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview  :: Grade
  , cultureFit  :: Grade
  , education   :: Degree
  } deriving Show

viable :: Candidate -> Bool
viable candidate = and tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [passedCoding, passedCultureFit, educationMin]

-- Using do-notation, a list is treated like a single value
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let verdict = if passed then "Passed" else "Failed"
  return verdict

-- Using Applicative, operations between two lists act on
-- every single combination of values between the two
applicativeExample = pure (*) <*> [1..4] <*> [5,6,7]

powersOf2 :: Int -> [Int]
powersOf2 n = do
  value <- [1..n]
  return (2^value)

powersOf2Example = powersOf2 10

powersOf2Map :: Int -> [Int]
powersOf2Map n = map (2^) [1..n]

powersOf2And3 :: Int -> [(Int,Int)]
powersOf2And3 n = do
  value <- [1..n]
  let powersOf2 = 2^value
  let powersOf3 = 3^value
  return (powersOf2, powersOf3)

powersOf2And3Example = powersOf2And3 5

allEvenOdds :: Int -> [(Int,Int)]
allEvenOdds n = do
  evenValue <- [2,4..n]
  oddValue <- [1,3..n]
  return (evenValue, oddValue)

allEvenOddsExample = allEvenOdds 6

squares :: [(Int,Int)]
squares = do
  value <- [1..10]
  return (value, value^2)

evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1..n]
  guard $ even value
  return value

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred xs = do
  x <- xs
  guard $ pred x
  return x

-- guard :: Alternative f => Bool -> f ()

-- This corresponds to [n**2 for n in range(10) if n**2 % 2 == 0]
evenSquares :: [Int]
evenSquares = do
  n <- [0..9]
  let nSquared = n^2
  guard $ even nSquared
  return nSquared

powersOf2' :: Int -> [Int]
powersOf2' n = [value^2 | value <- [1..n]]

powersOf2And3' :: Int -> [(Int,Int)]
powersOf2And3' n =
  [(p2, p3) | value <- [1..n], let p2 = 2^value, let p3 = 3^value]

allEvenOdds' :: Int -> [(Int,Int)]
allEvenOdds' n = [(evenValue, oddValue)
                 | evenValue <- [2,4..n]
                 , oddValue <- [1,3..n]]

evensGuard' :: Int -> [Int]
evensGuard' n = [value | value <- [1..n], even value]

strs = ["brown", "blue", "pink", "orange"]
strs' = ["Mr. " ++ capStr
        | str <- strs
        , let capStr = (\(c:cs) -> toUpper c : cs) str]

calendarDays = [[1..days]
               | month <- [1..12]
               , let days = if month == 2 then 28 else (if month `elem` [1,3,5,7,8,10,12] then 31 else 30)]

calendarDays' = do
  month <- [1..12]
  let days = if month == 2 then 28 else (if month `elem` [1,3,5,7,8,10,12] then 31 else 30)
  return [1..days]

calendarDays'' =
  [1..12] >>=
  (\month ->
    (\days -> return [1..days])
    (if month == 2 then 28 else (if month `elem` [1,3,5,7,8,10,12] then 31 else 30)))
