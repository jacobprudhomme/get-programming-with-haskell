#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

module Main where


{- In Prelude, head is defined as follows:
   head :: [a] -> a
   head (x:_) = x
   head []    = errorEmptyList "head"
-}

head :: Monoid a => [a] -> a
head (x:_) = x
head []    = mempty

-- Calling head on this throws an error. Must call
-- Main.head to differentiate from Prelude.head
example :: [[Int]]
example = []

length :: Int
length = 8

doubleLength :: Int
doubleLength = Main.length * 2 -- Just length would throw an error
