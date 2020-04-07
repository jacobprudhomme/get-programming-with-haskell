module Main where

import Primes


checkIfPrime :: IO ()
checkIfPrime = do
  putStrLn "Enter a number to check if it is prime:"
  nInput <- getLine
  let n = read nInput
  let msg = case isPrime n of
              Nothing    -> "Cannot determine whether " ++ nInput ++ " is prime"
              Just True  -> nInput ++ " is prime"
              Just False -> nInput ++ " is not prime"
  putStrLn msg

findPrimeFactors :: IO ()
findPrimeFactors = do
  putStrLn "Enter a number to find its prime factors:"
  nInput <- getLine
  let n = read nInput
  let msg = case primeFactors n of
              Nothing -> "Cannot determine the prime factors of " ++ nInput
              Just xs -> show xs ++ " are the prime factors of " ++ nInput
  putStrLn msg

main :: IO ()
main = do
  checkIfPrime
  findPrimeFactors
