#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import Data.List
import Data.Maybe
import Data.Semigroup

import qualified Data.Map as Map


file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2,  199.5), (3,  199.4)
        , (4, 198.9), (5,  199.0), (6,  200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)
        ]

file2 :: [(Int,Double)]
file2 = [ (11, 201.6), (12, 201.5), (13, 201.5)
        , (14, 203.5), (15, 204.9), (16, 207.1)
        , (18, 210.5), (20, 208.8)
        ]

file3 :: [(Int,Double)]
file3 = [ (10, 201.2), (11, 201.6), (12, 201.5)
        , (13, 201.5), (14, 203.5), (17, 210.5)
        , (24, 215.1), (25, 218.7)
        ]

file4 :: [(Int,Double)]
file4 = [ (26, 219.8), (27, 220.5), (28, 223.8)
        , (29, 222.8), (30, 223.8), (31, 221.7)
        , (32, 222.3), (33, 220.8), (34, 219.4)
        , (35, 220.1), (36, 220.6)
        ]

data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times vals = TS timeRange valsWithNoGaps
  where
    timeRange = [minimum times .. maximum times]
    timeValMap = Map.fromList $ zip times vals
    timeValMapLookup time = Map.lookup time timeValMap
    valsWithNoGaps = map timeValMapLookup timeRange

fileToTS :: [(Int,a)] -> TS a
fileToTS timeValPairs = createTS times vals
  where (times, vals) = unzip timeValPairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just val) = mconcat [show time, " | ", show val, "\n"]
showTVPair time Nothing    = mconcat [show time, " | NA\n"]

instance Show a => Show (TS a) where
  show (TS times vals) = mconcat rows
    where rows = zipWith showTVPair times vals

ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4

insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair kvMap (_, Nothing)    = kvMap
insertMaybePair kvMap (key, Just val) = Map.insert key val kvMap

combineTS :: TS a -> TS a -> TS a
combineTS ts1 (TS [] [])                      = ts1
combineTS (TS [] []) ts2                      = ts2
combineTS (TS times1 vals1) (TS times2 vals2) = TS timeRange allVals
  where
    bothTimes = mconcat [times1, times2]
    timeRange = [minimum bothTimes .. maximum bothTimes]
    ts1Map = foldl insertMaybePair Map.empty $ zip times1 vals1
    newMap = foldl insertMaybePair ts1Map $ zip times2 vals2
    lookupVal k = Map.lookup k newMap
    allVals = map lookupVal timeRange

instance Semigroup (TS a) where
  (<>) = combineTS

instance Monoid (TS a) where
  mempty = TS [] []

allTS :: TS Double
allTS = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs = total / count
  where
    total = realToFrac $ sum xs
    count = realToFrac $ length xs

tsMean :: Real a => TS a -> Maybe Double
tsMean (TS _ [])       = Nothing
tsMean (TS _ vals) =
  if all (== Nothing) vals
  then Nothing
  else Just avg
  where
    justVals = filter isJust vals
    unwrappedVals = map fromJust justVals
    avg = mean unwrappedVals

type CompareFunc a = a -> a -> a
type TSCompareFunc a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
makeTSCompare f = newF
  where
    newF (t1, Nothing) (t2, Nothing) = (t1, Nothing)
    newF pair1 (_, Nothing)          = pair1
    newF (_, Nothing) pair2          = pair2
    newF (t1, Just v1) (t2, Just v2) =
      if f v1 v2 == v1
      then (t1, Just v1)
      else (t2, Just v2)

tsCompareExample = makeTSCompare max (3, Just 200) (4, Just 10)

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] [])      = Nothing
compareTS f (TS times vals) =
  if all (== Nothing) vals
  then Nothing
  else Just best
  where
    tvPairs = zip times vals
    best = foldl (makeTSCompare f) (0, Nothing) tvPairs

tsMin :: Ord a => TS a -> Maybe (Int, Maybe a)
tsMin = compareTS min

tsMax :: Ord a => TS a -> Maybe (Int, Maybe a)
tsMax = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair _ Nothing = Nothing
diffPair Nothing _ = Nothing
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] [])      = TS [] []
diffTS (TS times vals) = TS times (Nothing : diffedVals)
  where
    shiftedVals = tail vals
    diffedVals = zipWith diffPair shiftedVals vals

ogMean = tsMean allTS
diffMean = tsMean $ diffTS allTS

maybeMean :: (Real a) => [Maybe a] -> Maybe Double
maybeMean vals =
  if Nothing `elem` vals
  then Nothing
  else Just avg
  where avg = mean $ map fromJust vals

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n   = []
movingAvg vals n =
  if length currVals == n
  then maybeMean currVals : movingAvg restVals n
  else []
  where
    currVals = take n vals
    restVals = tail vals

movingAvgTS :: (Real a) => TS a -> Int -> TS Double
movingAvgTS (TS [] []) _      = TS [] []
movingAvgTS (TS times vals) n = TS times smoothedVals
  where
    mvAvg = movingAvg vals n
    nothings = replicate (n `div` 2) Nothing
    smoothedVals = mconcat [nothings, mvAvg, nothings]

movingAvgExample = movingAvgTS allTS 3

median :: (Real a) => [a] -> Double
median xs =
  if odd count
  then realToFrac $ xs !! half
  else mean $ twoHalfPoints half
  where
    count = length xs
    half = count `div` 2
    twoHalfPoints halfLen = take 2 $ drop (halfLen - 1) xs

tsMedian :: Real a => TS a -> Maybe Double
tsMedian (TS _ []) = Nothing
tsMedian (TS _ vals) =
  if all (== Nothing) vals
  then Nothing
  else Just med
  where
    justVals = filter isJust vals
    unwrappedVals = map fromJust justVals
    med = median unwrappedVals

ogMedian = tsMedian allTS

maybeMedian :: Real a => [Maybe a] -> Maybe Double
maybeMedian vals =
  if Nothing `elem` vals
  then Nothing
  else Just med
  where med = median $ map fromJust vals

movingMedian :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingMedian [] n   = []
movingMedian vals n =
  if length currVals == n
  then maybeMedian currVals : movingMedian restVals n
  else []
  where
    currVals = take n vals
    restVals = tail vals

movingMedianTS :: (Real a) => TS a -> Int -> TS Double
movingMedianTS (TS [] []) _      = TS [] []
movingMedianTS (TS times vals) n = TS times smoothedVals
  where
    mvMed = movingMedian vals n
    nothings = replicate (n `div` 2) Nothing
    smoothedVals = mconcat [nothings, mvMed, nothings]

movingMedExample = movingMedianTS allTS 4

divPair :: Real a => Maybe a -> Maybe a -> Maybe Double
divPair _ Nothing = Nothing
divPair Nothing _ = Nothing
divPair (Just x) (Just y) = Just percentage
  where
    fracNum = realToFrac (x - y)
    fracDenom = realToFrac y
    percentage = 100 * fracNum / fracDenom

divTS :: Real a => TS a -> TS Double
divTS (TS [] [])      = TS [] []
divTS (TS times vals) = TS times (Nothing : divedVals)
  where
    shiftedVals = tail vals
    divedVals = zipWith divPair shiftedVals vals

divMedian = tsMedian $ divTS allTS

stdDev :: Real a => [a] -> Double
stdDev xs = sqrt $ sumOfSqrOfDistsFromMean / count
  where
    count = realToFrac $ length xs
    avg = mean xs
    distsFromMean = map (\x -> realToFrac x - avg) xs
    sqrOfDistsFromMean = map (^2) distsFromMean
    sumOfSqrOfDistsFromMean = realToFrac $ sum sqrOfDistsFromMean

tsStdDev :: Real a => TS a -> Maybe Double
tsStdDev (TS _ [])   = Nothing
tsStdDev (TS _ vals) =
  if all (== Nothing) vals
  then Nothing
  else Just stdDeviation
  where
    justVals = filter isJust vals
    unwrappedVals = map fromJust justVals
    stdDeviation = stdDev unwrappedVals

ogStdDev = tsStdDev allTS

newtype AddTS a = AddTS { fromAddTS :: TS a }
newtype SubTS a = SubTS { fromSubTS :: TS a }

insertAndAddMaybePair :: (Ord k, Num v) => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertAndAddMaybePair kvMap (_, Nothing)    = kvMap
insertAndAddMaybePair kvMap (key, Just val) =
  case Map.lookup key kvMap of
    Nothing     -> Map.insert key val kvMap
    Just oldVal -> Map.insert key (oldVal + val) kvMap

addTS :: Num a => AddTS a -> AddTS a -> AddTS a
addTS ts1 (AddTS (TS [] []))                              = ts1
addTS (AddTS (TS [] [])) ts2                              = ts2
addTS (AddTS (TS times1 vals1)) (AddTS (TS times2 vals2)) = AddTS $ TS timeRange allVals
  where
    bothTimes = mconcat [times1, times2]
    timeRange = [minimum bothTimes .. maximum bothTimes]
    ts1Map = foldl insertAndAddMaybePair Map.empty $ zip times1 vals1
    newMap = foldl insertAndAddMaybePair ts1Map $ zip times2 vals2
    lookupVal k = Map.lookup k newMap
    allVals = map lookupVal timeRange

instance Num a => Semigroup (AddTS a) where
  (<>) = addTS

instance Num a => Monoid (AddTS a) where
  mempty = AddTS $ TS [] []

insertAndSubMaybePair :: (Ord k, Num v) => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertAndSubMaybePair kvMap (_, Nothing)    = kvMap
insertAndSubMaybePair kvMap (key, Just val) =
  case Map.lookup key kvMap of
    Nothing     -> Map.insert key val kvMap
    Just oldVal -> Map.insert key (oldVal - val) kvMap

subTS :: Num a => SubTS a -> SubTS a -> SubTS a
subTS ts1 (SubTS (TS [] []))                              = ts1
subTS (SubTS (TS [] [])) ts2                              = ts2
subTS (SubTS (TS times1 vals1)) (SubTS (TS times2 vals2)) = SubTS $ TS timeRange allVals
  where
    bothTimes = mconcat [times1, times2]
    timeRange = [minimum bothTimes .. maximum bothTimes]
    ts1Map = foldl insertAndSubMaybePair Map.empty $ zip times1 vals1
    newMap = foldl insertAndSubMaybePair ts1Map $ zip times2 vals2
    lookupVal k = Map.lookup k newMap
    allVals = map lookupVal timeRange

{-
SubTS can't be made an instance of Semigroup or
Monoid because subtraction is not associative
-}

allTSAdded :: TS Double
allTSAdded = fromAddTS $ mconcat $ map AddTS [ts1, ts2, ts3, ts4]

someTSSubbed :: TS Double
someTSSubbed = fromSubTS $ subTS (SubTS ts2) (SubTS ts1)
