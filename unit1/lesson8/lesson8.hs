#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

length' []     = 0
length' (_:xs) = 1 + length xs

take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : rest
  where rest = take' (n - 1) xs

finiteCycle (x:xs) = x:xs ++ [x]

cycle' (x:xs) = x : cycle (xs ++ [x])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) ackermann'
  where ackermann' = ackermann m (n - 1)

collatz 1 = 1
collatz n = 1 +
  if even n
  then collatz (n `div` 2)
  else collatz ((n * 3) + 1)

reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = n1 + n2
  where
    n1 = fib (n - 1)
    n2 = fib (n - 2)

fastFib _ _ 0         = 0
fastFib n1 _ 1        = n1
fastFib _ n2 2        = n2
fastFib n1 n2 counter = fastFib n2 nextFib (counter - 1)
  where nextFib = n1 + n2
