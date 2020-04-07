#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import System.Environment
import System.IO


getCounts :: String -> (Int,Int,Int)
getCounts input = (charCount, wordCount, lineCount)
  where
    charCount = length input
    wordCount = length $ words input
    lineCount = length $ lines input

countsAsText :: (Int,Int,Int) -> String
countsAsText (cc,wc,lc) = unwords
  [ "chars: "
  , show cc
  , " words: "
  , show wc
  , " lines: "
  , show lc
  ]

-- Won't work on stats.dat, readFile doesn't close the handle
main' :: IO ()
main' = do
  args <- getArgs
  let fileName = head args
  input <- readFile fileName
  let summary = countsAsText $ getCounts input
  appendFile "stats.dat" $ mconcat [fileName, " ", summary, "\n"]
  putStrLn summary

-- Won't work on any file, handle closed before lazy read is executed
main'' :: IO ()
main'' = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  hClose file
  let summary = countsAsText $ getCounts input
  appendFile "stats.dat" $ mconcat [fileName,  " ", summary, "\n"]
  putStrLn summary

-- putStrLn forces evaluation of summary, which forces
-- input, thus it is ok to close the file after it
main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  file <- openFile fileName ReadMode
  input <- hGetContents file
  let summary = countsAsText $ getCounts input
  putStrLn summary
  hClose file
  appendFile "stats.dat" $ mconcat [fileName,  " ", summary, "\n"]
