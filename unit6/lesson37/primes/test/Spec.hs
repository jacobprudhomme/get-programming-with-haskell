import Data.Maybe
import Test.QuickCheck

import Primes


prop_validPrimesOnly n = if n < 2 || n >= length primes
                         then isNothing result
                         else isJust result
  where result = isPrime n

prop_primesArePrime n = result /= Just True || null divisors
  where
    result = isPrime n
    divisors = filter ((== 0) . (n `mod`)) [2..(n - 1)]

prop_nonPrimesAreComposite n = result /= Just False || not (null divisors)
  where
    result = isPrime n
    divisors = filter ((== 0) . (n `mod`)) [2..(n - 1)]

prop_factorsMakeOriginal n = isNothing result || product (fromJust result) == n
  where result = primeFactors n

prop_allFactorsPrime n = isNothing result || all (== Just True) resultsPrime
  where
    result = primeFactors n
    resultsPrime = map isPrime (fromJust result)

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs {maxSuccess = 1000} prop_primesArePrime
  quickCheckWith stdArgs {maxSuccess = 1000} prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
