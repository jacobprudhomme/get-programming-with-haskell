#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

fib 0 = 0
fib 1 = 1
fib n = n1 + n2
  where
    n1 = fib (n - 1)
    n2 = fib (n - 2)

main :: IO ()
main = do
  putStrLn "Which Fibonacci number do you want to see?"
  n <- getLine
  let result = fib (read n)
  putStrLn $ "That number is " ++ show result
