#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

module Palindrome
  ( isPalindrome
  , preprocess
  ) where

import Data.Char (toLower, isSpace, isPunctuation)

import qualified Data.Text as T


stripWhitespace :: String -> String
stripWhitespace = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowercase :: String -> String
toLowercase = map toLower

preprocess :: T.Text -> T.Text
preprocess = T.pack . stripWhitespace . stripPunctuation . toLowercase . T.unpack

isPalindrome :: T.Text -> Bool
isPalindrome str = ppStr == T.reverse ppStr
  where ppStr = preprocess str
