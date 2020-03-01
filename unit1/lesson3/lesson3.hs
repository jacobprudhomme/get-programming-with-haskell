#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

simple = \x -> x

double = \x -> 2 * x

sumOfSquaresOrSquareOfSum x y =
  if sumOfSquares >= squareOfSum
  then sumOfSquares
  else squareOfSum
    where
      sumOfSquares = x^2 + y^2
      squareOfSum  = (x + y)^2

sumOfSquaresOrSquareOfSum' x y =
  if (x^2 + y^2) >= ((x + y)^2)
  then x^2 + y^2
  else (x + y)^2

body sumOfSquares squareOfSum =
  if sumOfSquares >= squareOfSum
  then sumOfSquares
  else squareOfSum

sumOfSquaresOrSquareOfSum'' x y = body (x^2 + y^2) ((x + y)^2)

sumOfSquaresOrSquareOfSum''' x y =
  (\sumOfSquares squareOfSum ->
    if sumOfSquares >= squareOfSum
    then sumOfSquares
    else squareOfSum) (x^2 + y^2) ((x + y)^2)

doubleDouble x = (\dubs -> 2 * dubs) (2 * x)

sumOfSquaresOrSquareOfSum'''' x y =
  let sumOfSquares = x^2 + y^2
      squareOfSum  = (x + y)^2
  in
    if sumOfSquares >= squareOfSum
    then sumOfSquares
    else squareOfSum

overwrite x =
  let x = 2
  in
    let x = 3
    in
      let x = 4
      in x

overwrite' x = (\x -> (\x -> (\x -> x) 4) 3) 2

x = 4

add1 y = y + x

add2 y = (\x -> y + x) 3

add3 y = (\y -> (\x -> y + x) 1) 2

square x = (\x -> x^2) x

fn n = (\n ->
  if even n
  then n - 2
  else 3 * n + 1) n

counter x =
  let x = x + 1
  in
    let x = x + 1
    in x

counter' x =
  (\x -> (\x -> x) (x + 1)) (x + 1)
