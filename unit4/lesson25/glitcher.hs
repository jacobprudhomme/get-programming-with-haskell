#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.Environment
import System.IO
import System.Random

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


main' :: IO ()
main' = do
  args <- getArgs
  let fileName = head args
  image <- B.readFile fileName
--  glitchedImage <- return image
  let glitchedImage = image
  let glitchedFileName = mconcat ["glitched_", fileName]
  B.writeFile glitchedFileName glitchedImage
  putStrLn "Done!"

intToChar :: Int -> Char
intToChar n = toEnum safeN
  where safeN = n `mod` 255

intToBS :: Int -> B.ByteString
intToBS n = BC.pack [intToChar n]

replaceByte :: Int -> Int -> B.ByteString -> B.ByteString
replaceByte loc c bytes = mconcat [before, newByte, after]
  where
    (before, rest) = B.splitAt loc bytes
    after = B.drop 1 rest
    newByte = intToBS c

randomReplaceByte :: B.ByteString -> IO B.ByteString
randomReplaceByte bytes = do
  let bytesLen = B.length bytes
  location <- randomRIO (1, bytesLen)
  charVal <- randomRIO (0, 255)
  return $ replaceByte location charVal bytes

main'' :: IO ()
main'' = do
  args <- getArgs
  let fileName = head args
  image <- B.readFile fileName
  glitchedImage <- randomReplaceByte image
  let glitchedFileName = "glitched_" ++ fileName
  B.writeFile glitchedFileName glitchedImage
  putStrLn "Done!"

randomChar :: IO Char
randomChar = do
  charVal <- randomRIO (0, 255)
  return $ toEnum charVal

sortSection :: Int -> Int -> B.ByteString -> B.ByteString
sortSection start size bytes = mconcat [before, sorted, after]
  where
    (before, rest) = B.splitAt start bytes
    (toBeSorted, after) = B.splitAt size rest
    sorted = B.reverse $ B.sort toBeSorted

randomSortSection :: B.ByteString -> IO B.ByteString
randomSortSection bytes = do
  let sectionLen = 25
  let bytesLen = B.length bytes
  start <- randomRIO (0, bytesLen - sectionLen)
  return $ sortSection start sectionLen bytes

main''' :: IO ()
main''' = do
  args <- getArgs
  let fileName = head args
  image <- B.readFile fileName
  glitchedImage <- randomSortSection image
  let glitchedFileName = "glitched_" ++ fileName
  B.writeFile glitchedFileName glitchedImage
  putStrLn "Done!"

main4 :: IO ()
main4 = do
  args <- getArgs
  let fileName = head args
  image <- B.readFile fileName
  glitchedImage1 <- randomReplaceByte image
  glitchedImage2 <- randomSortSection glitchedImage1
  glitchedImage3 <- randomReplaceByte glitchedImage2
  glitchedImage4 <- randomSortSection glitchedImage3
  glitchedImage5 <- randomReplaceByte glitchedImage4
  let glitchedFileName = "glitched_" ++ fileName
  B.writeFile glitchedFileName glitchedImage5
  putStrLn "Done!"

main5 :: IO ()
main5 = do
  args <- getArgs
  let fileName = head args
  image <- B.readFile fileName
  glitchedImage <- foldM
    (\bytes glitch -> glitch bytes)
    image
    [ randomReplaceByte
    , randomSortSection
    , randomReplaceByte
    , randomSortSection
    , randomReplaceByte
    ]
  let glitchedFileName = "glitched_" ++ fileName
  B.writeFile glitchedFileName glitchedImage
  putStrLn "Done!"

reverseSection :: Int -> Int -> B.ByteString -> B.ByteString
reverseSection start size bytes = mconcat [before, reversed, after]
  where
    (before, rest) = B.splitAt start bytes
    (toBeReversed, after) = B.splitAt size rest
    reversed = B.reverse toBeReversed

randomReverseSection :: B.ByteString -> IO B.ByteString
randomReverseSection bytes = do
  let sectionLen = 25
  let bytesLen = B.length bytes
  start <- randomRIO (0, bytesLen - sectionLen)
  return $ reverseSection start sectionLen bytes

glitchActions :: [B.ByteString -> IO B.ByteString]
glitchActions =
  [ randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomReverseSection
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  image <- B.readFile fileName
  glitchedImage <- foldM
    (\bytes glitch -> glitch bytes)
    image
    glitchActions
  let glitchedFileName = "glitched_" ++ fileName
  B.writeFile glitchedFileName glitchedImage
  putStrLn "Done!"
