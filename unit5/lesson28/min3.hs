#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20


minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree v1 v2 v3 = min v1 $ min v2 v3

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
  putStrLn "Enter three numbers:"
  minInt <- minOfInts
  putStrLn $ show minInt ++ " is the smallest"

val1 :: Maybe Int
val1 = Just 10

val2 :: Maybe Int
val2 = Just 3

val3 :: Maybe Int
val3 = Just 6

qc = minOfThree <$> val1 <*> val2 <*> val3
