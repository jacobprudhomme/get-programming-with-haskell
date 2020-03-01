#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import Data.Char
import System.Environment
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


capitalize :: T.Text -> T.Text
capitalize = T.pack . map toUpper . T.unpack

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  contents <- TIO.readFile fileName
  TIO.writeFile fileName $ capitalize contents
