module Main where

import Data.Text as T
import Data.Text.IO as TIO

import Palindrome


main :: IO ()
main = do
  TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
  str <- TIO.getLine
  let msg = if isPalindrome str
            then "It is!"
            else "It's not!"
  TIO.putStrLn msg
