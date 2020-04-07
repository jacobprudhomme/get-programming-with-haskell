#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import System.Environment
import System.IO


main :: IO ()
main = do
  args <- getArgs
  let srcFile = head args
  let destFile = head $ tail args
  contents <- readFile srcFile
  writeFile destFile contents
  putStrLn "Done!"
