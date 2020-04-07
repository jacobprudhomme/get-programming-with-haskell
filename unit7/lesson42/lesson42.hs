#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.STRef


aLargeList :: [Int]
aLargeList = [1..10000000]

aLargeArray :: UArray Int Int
aLargeArray = array (0,9999999) []

aLargeListDoubled :: [Int]
aLargeListDoubled = map (*2) aLargeList

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3, True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1..10] $ repeat True

qcArray :: UArray Int Bool
qcArray = array (0,4) [(2,True), (3,True)]

-- All values will be initialized to 0
beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) []

-- Same as above, but explicit
beansInBuckets' :: UArray Int Int
beansInBuckets' = array (0,3) $ zip [0..3] $ repeat 0

-- Put some beans in buckets 1 and 3
updatedBIB1 :: UArray Int Int
updatedBIB1 = beansInBuckets // [(1,5), (3,6)]

-- Add 2 beans to every bucket
updatedBIB2 :: UArray Int Int
updatedBIB2 = accum (+) updatedBIB1 $ zip [0..3] $ repeat 2

doubleBeans :: UArray Int Int
doubleBeans = accum (*) updatedBIB2 $ zip [0..3] $ repeat 2

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
  let end = length vals - 1
  stArray <- newArray (0,end) 0
  forM_ [0..end] $ \i -> do
    let val = vals !! i
    writeArray stArray i val
  return stArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

-- Usually written all at once, like this
listToUArray' :: [Int] -> UArray Int Int
listToUArray' vals = runSTUArray $ do
  let end = length vals - 1
  myArray <- newArray (0,end) 0
  forM_ [0..end] $ \i -> do
    let val = vals !! i
    writeArray myArray i val
  return myArray

swapST :: (Int,Int) -> (Int,Int)
swapST (x, y) = runST $ do
  x' <- newSTRef x
  y' <- newSTRef y
  writeSTRef x' y
  writeSTRef y' x
  xFinal <- readSTRef x'
  yFinal <- readSTRef y'
  return (xFinal, yFinal)

myData :: UArray Int Int
myData = listArray (0,5) [7,6,4,8,10,2]

myData' :: UArray Int Int
myData' = listToUArray [7,6,4,8,10,2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = snd $ bounds myArray
  forM_ [1..end] $ \i ->
    forM_ [0..(end - i)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray

bsExample :: UArray Int Int
bsExample = bubbleSort myData

crossover :: (UArray Int Int,UArray Int Int) -> Int -> UArray Int Int
crossover (array1, array2) cutoff = runSTUArray $ do
  stArray1 <- thaw array1
  let end = snd $ bounds array1
  forM_ [cutoff..end] $ \i -> do
    let val = array2 ! i
    writeArray stArray1 i val
  return stArray1

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros myArray = runSTUArray $ do
  stArray <- thaw myArray
  let start = fst $ bounds myArray
  let end = snd $ bounds myArray
  forM_ [start..end] $ \i -> do
    val <- readArray stArray i
    when (val == 0) $ writeArray stArray i (-1)
  return stArray
