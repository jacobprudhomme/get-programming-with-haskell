#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import Control.Monad
import System.Environment


main' :: IO ()
main' = do
  args <- getArgs
  -- map putStrLn args doesn't work in an IO context
  -- mapM putStrLn args returns [()], not ()
  mapM_ putStrLn args -- '_' usually denotes result is discarded

main'' :: IO ()
main'' = do
  multiple <- mapM (\_ -> getLine) [1..3]
  mapM_ putStrLn multiple

main :: IO ()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read $ head args
                    else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print $ sum ints

replicateM' :: Int -> IO a -> IO [a]
replicateM' n action = mapM (\_ -> action) [1..n]
