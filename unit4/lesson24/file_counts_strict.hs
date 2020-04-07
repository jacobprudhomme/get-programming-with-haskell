#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


getCounts :: T.Text -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = T.length input
    wordCount = length $ T.words input
    lineCount = length $ T.lines input

countsAsText :: (Int,Int,Int) -> T.Text
countsAsText (cc,wc,lc) = T.pack $ unwords
  [ "chars: "
  , show cc
  , " words: "
  , show wc
  , " lines: "
  , show lc
  ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  input <- TIO.readFile fileName
  let summary = countsAsText $ getCounts input
  TIO.appendFile "stats.dat" $
    mconcat [(T.pack fileName),  " ", summary, "\n"]
  TIO.putStrLn summary
