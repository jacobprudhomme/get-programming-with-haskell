#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

myGCD a b =
  if remainder == 0
  then b
  else myGCD b remainder
  where remainder = a `mod` b

sayAmount n =
  case n of
    1 -> "one"
    2 -> "two"
    _ -> "a bunch"

sayAmount' 1 = "one"
sayAmount' 2 = "two"
sayAmount' _ = "a bunch"

isEmpty [] = True
isEmpty _  = False

myHead (x:xs) = x
myHead []     = error "Cannot take head of empty list"

myTail (_:xs) = xs
myTail []     = error "Cannot take tail of empty list"

myGCD' a 0 = a
myGCD' a b = myGCD b remainder
  where remainder = a `mod` b
