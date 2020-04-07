#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import System.IO


-- openFile "stuff.txt" ReadMode
main' :: IO ()
main' = do
  file <- openFile "hello.txt" ReadMode
  hClose file
  putStrLn "Done!"

main'' :: IO ()
main'' = do
  helloFile <- openFile "hello.txt" ReadMode
  fstLine <- hGetLine helloFile
  putStrLn fstLine
  sndLine <- hGetLine helloFile
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile sndLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "Done!"

main :: IO ()
main = do
  helloFile <- openFile "hello.txt" ReadMode
  atEnd <- hIsEOF helloFile
  fstLine <-
    if not atEnd
    then hGetLine helloFile
    else return "Empty"
  putStrLn fstLine
  atEnd <- hIsEOF helloFile
  sndLine <-
    if not atEnd
    then hGetLine helloFile
    else return ""
  goodbyeFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodbyeFile sndLine
  hClose helloFile
  hClose goodbyeFile
  putStrLn "Done!"
