#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Palindrome

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
{-
import qualified Palindrome


isPalindrome :: String -> Bool
isPalindrome str = str == reverse str
-}

main :: IO ()
main = do
  TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome"
  str <- TIO.getLine
--let msg = if Palindrome.isPalindrome str -- Does not use isPalindrome defined in this file
  let msg = if isPalindrome str
            then "It is!"
            else "It's not!"
  putStrLn msg
