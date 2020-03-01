#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

mystery1 :: Int -> Int -> Int
mystery1 val1 val2 = (val1 + val2 + val3)^2
  where val3 = 3

mystery2 :: Int -> Int -> IO Int
mystery2 val1 val2 = do
  putStrLn "Enter a number"
  val3Input <- getLine
  let val3 = read val3Input
  return ((val1 + val2 + val3)^2)

safeValue = (mystery1 2 4) + (mystery1 5 6)

-- Doesn't compile, because pure (+) can't use impure inputs
-- unsafeValue = (mystery2 2 4) + (mystery2 2 4)
