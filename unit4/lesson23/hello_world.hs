#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO


helloPerson :: T.Text -> T.Text
helloPerson name = "Hello " <> name <> "!"

main :: IO ()
main = do
  TIO.putStrLn "Hello! What's your name?"
  name <- TIO.getLine
  let msg = helloPerson name
  TIO.putStrLn msg

toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

main' :: IO ()
main' = do
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  TLIO.putStrLn $ TL.pack $ show $ sum numbers
