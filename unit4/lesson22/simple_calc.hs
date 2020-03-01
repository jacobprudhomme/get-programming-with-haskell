#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import Data.List
import Data.List.Split
import Data.Maybe

import qualified Data.Map as Map


breakApartSides = map read . splitOneOf "+*"

applyOp :: String -> Int
applyOp op
  | "+" `isInfixOf` op = sum $ breakApartSides op
  | otherwise          = product $ breakApartSides op

main :: IO ()
main = do
  userInput <- getContents
  let ops = lines userInput
  let res = map applyOp ops
  mapM_ print res

famousQuotes = Map.fromList
  [ (1, "Ayy lmao")
  , (2, "Y'already know what it is")
  , (3, "Bicc boi")
  , (4, "Dummy thicc")
  , (5, "Bernie 2020")
  ]

lookup' k = fromJust $ Map.lookup k famousQuotes

getQuotes ("n":_) = ["Bye-bye for now"]
getQuotes (k:ks)  = lookup' (read k) : getQuotes ks

main' :: IO ()
main' = do
  userInput <- getContents
  let commands = lines userInput
  let quotes = getQuotes commands
  mapM_ putStrLn quotes
