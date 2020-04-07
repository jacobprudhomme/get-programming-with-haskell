#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import System.Random


minRoll :: Int
minRoll = 1

maxRoll :: Int
maxRoll = 6

main :: IO ()
main = do
  dieRoll <- randomRIO (minRoll, maxRoll)
  putStrLn $ show dieRoll
