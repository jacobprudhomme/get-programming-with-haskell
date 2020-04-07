module Lib where

import Data.Char (isPunctuation, isSpace)
import Data.Text as T


stripWhitespace :: Text -> Text
stripWhitespace = T.filter (not . isSpace)

stripPunctuation :: Text -> Text
stripPunctuation = T.filter (not . isPunctuation)

preprocess :: Text -> Text
-- v1: preprocess str = filter (not . (`elem` ['.', '!', '\187'])) str
-- v2: preprocess str = T.filter (not . isPunctuation) str
preprocess = stripWhitespace . stripPunctuation . T.toLower

isPalindrome :: Text -> Bool
-- v1: isPalindrome str = str == reverse str
isPalindrome str = ppStr == T.reverse ppStr
  where ppStr = preprocess str
