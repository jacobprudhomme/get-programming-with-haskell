#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import System.Environment

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


-- Try showing this in GHCi
nagarjunaBS :: B.ByteString
nagarjunaBS = "नागार्जुन"

nagarjunaText :: T.Text
nagarjunaText = "नागार्जुन"

nagarjunaBS' :: B.ByteString
nagarjunaBS' = BC.pack $ T.unpack nagarjunaText
-- Do TIO.putStrLn $ T.pack $ BC.unpack nagarjunaBS' to test

nagarjunaSafe :: B.ByteString
nagarjunaSafe = TE.encodeUtf8 nagarjunaText
-- Do TIO.putStrLn $ TE.decodeUtf8 nagarjunaSafe to test

diffLength :: B.ByteString -> Int
diffLength bytes = byteLen - charLen
  where
    byteLen = B.length bytes
    charLen = T.length $ TE.decodeUtf8 bytes

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  contents <- B.readFile fileName
  let diff = diffLength contents
  print diff
