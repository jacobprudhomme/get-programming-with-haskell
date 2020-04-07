module Primes
  ( isPrime
  , primeFactors
  , primes
  , sieve
  ) where


sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve factorsRemoved
  where factorsRemoved = filter ((/= 0) . (`mod` nextPrime)) rest

primes :: [Int]
primes = sieve [2..10000]

isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2              = Nothing
  | n >= length primes = Nothing
  | otherwise          = Just $ n `elem` primes

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 []                 = []
unsafePrimeFactors n []                 = []
unsafePrimeFactors n (nextPrime:primes) =
  if n `mod` nextPrime == 0
  then nextPrime : unsafePrimeFactors (n `div` nextPrime)
                                      (nextPrime : primes)
  else unsafePrimeFactors n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2              = Nothing
  | n >= length primes = Nothing
  | otherwise          = Just $ unsafePrimeFactors n primesUpToN
  where primesUpToN = filter (<= n) primes
