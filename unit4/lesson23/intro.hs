#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T


firstWord :: String
firstWord = "littyboi"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- myWord :: T.Text
-- myWord = "dog"
-- Without OverloadedStrings, throws an error
-- because a string literal is of type String

myNum1 :: Int
myNum1 = 3

myNum2 :: Integer
myNum2 = 3

myNum3 :: Double
myNum3 = 3
