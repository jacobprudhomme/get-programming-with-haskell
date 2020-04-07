#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T


aWord :: T.Text
aWord = "Cheese"

main :: IO ()
main = print aWord

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

textLines :: [T.Text]
textLines = T.lines sampleInput

textUnlines :: T.Text
textUnlines = T.unlines textLines

someText :: T.Text
someText = "Some\ntext for\t you"

textWords :: [T.Text]
textWords = T.words someText

textUnwords :: T.Text
textUnwords = T.unwords textWords

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

splitText :: [T.Text]
splitText = T.splitOn breakText exampleText

intercalatedText :: T.Text
intercalatedText = T.intercalate breakText splitText

-- ++ won't work on Text

textMonoid :: T.Text
textMonoid = mconcat ["some", " ", "text"]

textSemigroup :: T.Text
textSemigroup = "some" <> " " <> "text"

lines' :: T.Text -> [T.Text]
lines' t = T.splitOn "\n" t

unlines' :: [T.Text] -> T.Text
unlines' ts = T.intercalate "\n" ts
