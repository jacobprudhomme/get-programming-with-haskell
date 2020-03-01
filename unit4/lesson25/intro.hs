#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


sampleBytes :: B.ByteString
sampleBytes = "Hello!"

{- This won't work without importing ByteString.Char8.
   After importing that alongside ByteString, now functions
   from both modules will use the same ByteString type,
   instead of B.ByteString and BC.ByteString separately.
-}
-- sampleString :: String
-- sampleString = B.unpack sampleBytes

sampleString :: String
sampleString = BC.unpack sampleBytes

bsInt :: B.ByteString
bsInt = "6"

bsToInt :: B.ByteString -> Int
bsToInt = read . BC.unpack
