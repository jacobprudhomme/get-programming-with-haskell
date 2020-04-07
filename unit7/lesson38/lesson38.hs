#!/usr/bin/env stack
-- stack ghci --ghc-options=-Wall --resolver lts-14.20

import Data.Char (isDigit)


-- head [1] works
-- head [] throws an error

-- Even with -Wall, the compiler won't catch
-- that this could cause a runtime error
take' :: Int -> [a] -> [a]
take' 0 _  = []
take' n xs = head xs : take' (n - 1) (tail xs)

-- This will throw an error at compile-time if
-- the 2nd line of the definition is removed
takePM :: Int -> [a] -> [a]
takePM 0 _      = []
takePM _ []     = []
takePM n (x:xs) = x : takePM (n - 1) xs

head' :: [a] -> a
head' []    = error "Empty list"
head' (x:_) = x

partialFnExample :: Double
partialFnExample = 2 / 0

-- maximum fails on []
-- succ on the last element of an Enum (maxBound)
-- sum fails on infinite lists

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

maybeHeadExample1 :: Maybe Int
maybeHeadExample1 = maybeHead [1]
maybeHeadExample2 :: Maybe Int
maybeHeadExample2 = maybeHead []

maybeHeadExample3 :: Maybe Int
maybeHeadExample3 = (+2) <$> maybeHead [1]
maybeHeadExample4 :: Maybe Int
maybeHeadExample4 = (+2) <$> maybeHead []

maybeHeadExample5 :: Maybe [Int]
maybeHeadExample5 = (:) <$> maybeHead [1,2,3] <*> Just []
maybeHeadExample6 :: Maybe [Int]
maybeHeadExample6 = (:) <$> maybeHead [] <*> Just []

-- This is still partial, as the compiler will let you know!
{-
takeSafer :: Int -> Maybe [a] -> Maybe [a]
takeSafer 0 _         = Just []
takeSafer n (Just xs) = (:) <$> maybeHead xs
                             <*> takeSafer (n - 1) (Just (tail xs))

takeSaferExample1 :: Maybe [Int]
takeSaferExample1 = takeSafer 3 (Just [1,2,3])
takeSaferExample2 :: Maybe [Int]
takeSaferExample2 = takeSafer 6 (Just [1,2,3])
-}

primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

isPrime' :: Int -> Maybe Bool
isPrime' n
  | n < 2     = Nothing
  | n > maxN  = Nothing
  | otherwise = Just $ n `elem` primes

-- The Nothing taken by head would be of type Maybe Int
oddList :: [Maybe Int]
oddList = [Nothing]

eitherHead :: [a] -> Either String a
eitherHead []    = Left "Cannot take head of an empty list"
eitherHead (x:_) = Right x

intExample :: [Int]
intExample = [1,2,3]

emptyIntExample :: [Int]
emptyIntExample = []

charExample :: [Char]
charExample = "cat"

emptyCharExample :: [Char]
emptyCharExample = []

eitherHeadExample1 :: Either String Int
eitherHeadExample1 = eitherHead intExample
eitherHeadExample2 :: Either String Int
eitherHeadExample2 = eitherHead emptyIntExample
eitherHeadExample3 :: Either String Char
eitherHeadExample3 = eitherHead charExample
eitherHeadExample4 :: Either String Char
eitherHeadExample4 = eitherHead emptyCharExample

eitherHeadExample5 :: Either String Int
eitherHeadExample5 = (+1) <$> eitherHead intExample
eitherHeadExample6 :: Either String Int
eitherHeadExample6 = (+1) <$> eitherHead emptyIntExample

eitherHeadExample7 :: Either String Int
eitherHeadExample7 = (+) <$> eitherHead intExample <*> eitherHead (tail intExample)

isPrime'' :: Int -> Either String Bool
isPrime'' n
  | n < 2     = Left "Numbers less than 2 are not candidates for primality"
  | n > maxN  = Left "Value exceeds limit of prime checker"
  | otherwise = Right $ n `elem` primes

isPrimeExample1 :: Either String Bool
isPrimeExample1 = isPrime'' 5
isPrimeExample2 :: Either String Bool
isPrimeExample2 = isPrime'' 6
isPrimeExample3 :: Either String Bool
isPrimeExample3 = isPrime'' 100
isPrimeExample4 :: Either String Bool
isPrimeExample4 = isPrime'' (-29)

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceeds max bound"
  show InvalidValue = "Value is not a candidate for primality"

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2     = Left InvalidValue
  | n > maxN  = Left TooLarge
  | otherwise = Right $ n `elem` primes

isPrimeExample5 :: Either PrimeError Bool
isPrimeExample5 = isPrime 99
isPrimeExample6 :: Either PrimeError Bool
isPrimeExample6 = isPrime 0

displayResult :: Either PrimeError Bool -> String
displayResult (Right True)  = "It is prime"
displayResult (Right False) = "It is composite"
displayResult (Left pError) = show pError

main :: IO ()
main = do
  putStrLn "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  putStrLn $ displayResult result

addStrInts :: String -> String -> Either String Int
addStrInts n1 n2
  | isNum n1 && isNum n2 = Right $ read n1 + read n2
  | isNum n2             = Left "The first argument is not a valid number"
  | isNum n1             = Left "The second argument is not a valid number"
  | otherwise            = Left "Neither argument is a valid number"
  where isNum = all isDigit

succ' :: (Bounded a, Enum a, Eq a) => a -> Maybe a
succ' x
  | x == maxBound = Nothing
  | otherwise     = Just $ succ x

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

last' :: [a] -> Either String a
last' [] = Left "Cannot take last element of empty list"
last' xs
  | length xs > 1000 = Left "Cannot take last element of infinite list"
  | otherwise        = Right $ last xs
