#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import Data.Char


addAnA []     = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

squareAll []     = []
squareAll (x:xs) = (x^2) : squareAll xs

map' _ []     = []
map' f (x:xs) = (f x) : map' f xs

filter' _ []        = []
filter' pred (x:xs) =
  if pred x
  then x : filter' pred xs
  else filter' pred xs

remove _ []        = []
remove pred (x:xs) =
  if pred x
  then remove pred xs
  else x : remove pred xs

product' xs = foldl (*) 1 xs

concatAll xs = foldl (++) "" xs

sumOfSquares xs = foldl (+) 0 (map (^2) xs)

squareOfSum xs = (foldl (+) 0 xs)^2

rcons x y = y:x

reverse' xs = foldl rcons [] xs

foldl' _ init []     = init
foldl' f init (x:xs) = foldl' f newInit xs
  where newInit = f init x

foldr' _ acc []     = acc
foldr' f acc (x:xs) = f x toBeAcc'd
  where toBeAcc'd = foldr' f acc xs

elem' x xs = length (filter (== x) xs) > 0

isPalindrome word = normalizedWord == reverse normalizedWord
  where normalizedWord = map toLower $ filter (/= ' ') word

harmonic n = foldr (+) 0 truncatedSequence
  where
    sequence          = map (1 /) [1..]
    truncatedSequence = take n sequence
