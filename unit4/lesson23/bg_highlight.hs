#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


dharma :: T.Text
dharma = "धर्म"

bgText :: T.Text
bgText = "श्रेयान्स्वधर्मो विगुणः परधर्मात्स्वनुष्ठितात्।स्वधर्मे निधनं श्रेयः परधर्मो भयावहः"

highlight :: T.Text -> T.Text -> T.Text
highlight query text = T.intercalate highlightedText segments
  where
    segments = T.splitOn query text
    highlightedText = mconcat ["{", query, "}"]

main :: IO ()
main = TIO.putStrLn $ highlight dharma bgText
