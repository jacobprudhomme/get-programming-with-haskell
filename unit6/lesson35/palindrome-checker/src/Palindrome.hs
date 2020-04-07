module Palindrome
  ( isPalindrome
  ) where

import Data.Char (isSpace, isPunctuation)

import qualified Data.Text as T


stripWhitespace :: T.Text -> T.Text
stripWhitespace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

preprocess :: T.Text -> T.Text
preprocess = stripWhitespace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome str = ppStr == T.reverse ppStr
  where ppStr = preprocess str
