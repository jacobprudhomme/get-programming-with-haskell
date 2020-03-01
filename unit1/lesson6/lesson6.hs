#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

paExample = (!!) "dog"

paExample' = ("dog" !!)

paExample'' = (!! 2)

isPalindrome word = word == reverse word

respond phrase =
  if '!' `elem` phrase
  then "wow!"
  else "uh... ok"

takeLast n list = reverse $ take n $ reverse list

ones n = take n $ cycle [1]

assignToGroups n list = zip groups list
  where groups = cycle [1..n]

repeat' n = cycle [n]

subseq start end list = drop start $ take end list

inFirstHalf n list = n `elem` take half list
  where half = (length list) `div` 2
