{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isPunctuation, isSpace)
import Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances

import Lib


assert :: Bool -> String -> String -> IO ()
assert test passStmt failStmt = if test
                                then putStrLn passStmt
                                else putStrLn failStmt

main' :: IO ()
main' = do
  putStrLn "Running tests..."
  assert (isPalindrome "racecar") "Passed 'racecar'" "FAIL: 'racecar'"
  assert (isPalindrome "racecar.") "Passed 'racecar.'" "FAIL: 'racecar.'"
  assert (isPalindrome "racecar!") "Passed 'racecar!'" "FAIL: 'racecar!'"
  assert (isPalindrome ":racecar:") "Passed ':racecar:'" "FAIL: ':racecar:'"
  assert (not $ isPalindrome "cat") "Passed 'cat'" "FAIL: 'cat'"
  putStrLn "Done!"

prop_punctuationInvariant str = preprocess str == preprocess noPuncStr
  where noPuncStr = T.filter (not . isPunctuation) str

prop_whitespaceInvariant str = preprocess str == preprocess noWhtspcStr
  where noWhtspcStr = T.filter (not . isSpace) str

prop_capitalizationInvariant str = preprocess str == preprocess noCapsStr
  where noCapsStr = T.toLower str

prop_reverseInvariant str = isPalindrome str == isPalindrome revStr
  where revStr = T.reverse str

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_whitespaceInvariant
  quickCheckWith stdArgs {maxSuccess = 1000} prop_capitalizationInvariant
  quickCheck prop_reverseInvariant
  putStrLn "Done!"
