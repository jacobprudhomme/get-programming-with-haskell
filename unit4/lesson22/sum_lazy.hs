#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import Data.List.Split


-- Run this in GHCi to make it so that Ctrl-D can be used for EOF:
-- import System.IO
-- hSetBuffering stdin LineBuffering

main' :: IO ()
main' = do
  userInput <- getContents
  mapM_ print userInput

reverseInput :: IO ()
reverseInput = do
  userInput <- getContents
  putStrLn $ reverse userInput

sampleData = ['6', '2', '\n', '2', '1', '\n']

stickTogether = lines sampleData

lines' = splitOn "\n"

stickTogether' = lines' sampleData

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print $ sum numbers

sumOfSquares :: IO ()
sumOfSquares = do
  userInput <- getContents
  let numbers = toInts userInput
  let squares = map (^2) numbers
  print $ sum squares
