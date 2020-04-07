#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

simple x = x

x = 2
-- x = 3

calcChange owed given =
  if given - owed > 0
  then given - owed
  else 0

calcChange' owed given = if change > 0 then change else 0
  where change = given - owed

doublePlusTwo = doubleX + 2
  where doubleX = 2 * x

inc n = n + 1

double n = 2 * n

square n = n ^ 2

fn n =
  if even n
  then n - 2
  else 3 * n + 1

fn' n =
  if n `mod` 2 == 0
  then n - 2
  else 3 * n + 1
