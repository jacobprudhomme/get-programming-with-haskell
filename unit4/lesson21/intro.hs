#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let msg = helloPerson name
  putStrLn msg
