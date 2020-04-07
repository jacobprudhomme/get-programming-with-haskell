#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

-- cup flOz = \_ -> flOz
cup flOz = \message -> message flOz

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = cup $
  if ozDiff >= 0
  then ozDiff
  else 0
  where
    flOz   = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

coffeeCup = cup 12
afterASip = drink coffeeCup 1
afterTwoSips = drink afterASip 2
afterAGulp = drink afterTwoSips 4
afterBigGulp = drink afterAGulp 8
afterManySips = foldl drink coffeeCup [1,1,1,1,1]
