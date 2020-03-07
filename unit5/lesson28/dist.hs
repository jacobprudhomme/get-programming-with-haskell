#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import qualified Data.Map as Map


type LatLong = (Double,Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList
  [ ("Arkham", (42.6054, -70.7829))
  , ("Innsmouth", (42.8250, -70.8150))
  , ("Carcosa", (29.9714, -90.7694))
  , ("New York", (40.7776, -73.9691))
  ]

toRadians :: Double -> Double
toRadians degs = degs * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine loc1 loc2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads loc1
    (rlat2, rlong2) = latLongToRads loc2
    diffLat = rlat2 - rlat1
    diffLong = rlong2 - rlong1
    a = (sin (diffLat / 2))^2 + (cos rlat1) * (cos rlat2) * (sin (diffLong / 2))^2
    c = 2 * atan2 (sqrt a) (sqrt (1 - a))
    earthRadius = 3961.0

distExample = haversine (40.7776, -73.9691) (42.6054, -70.7829)

printDist :: Maybe Double -> IO ()
printDist Nothing     = putStrLn "Error: invalid city entered"
printDist (Just dist) = putStrLn $ show dist ++ " miles"

haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _               = Nothing
haversineMaybe _ Nothing               = Nothing
haversineMaybe (Just val1) (Just val2) = Just $ haversine val1 val2

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just n1) (Just n2) = Just $ n1 + n2
addMaybe _ _                 = Nothing

distFromNY :: LatLong -> Double
distFromNY = haversine newYork
  where newYork = (40.7776, -73.9691)

maybeInc :: Maybe (Int -> Int)
maybeInc = (+) <$> Just 1

appExample = maybeInc <*> Just 5
appExample' = maybeInc <*> Nothing
appExample'' = (++) <$> Just "cats" <*> Just " and dogs"
appExample''' = (++) <$> Nothing <*> Just " and dogs"
appExample4 = (++) <$> Just "cats" <*> Nothing

val1 = Just 10
val2 = Just 5

qc = (*) <$> val1 <*> val2
qc' = div <$> val1 <*> val2
qc'' = mod <$> val1 <*> val2

startingCity = Map.lookup "Carcosa" locationDB
destCity = Map.lookup "Innsmouth" locationDB
dist = haversine <$> startingCity <*> destCity

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  start <- getLine
  let startingCity = Map.lookup start locationDB
  putStrLn "Enter the destination city name:"
  dest <- getLine
  let destCity = Map.lookup dest locationDB
  let dist = haversine <$> startingCity <*> destCity
  printDist dist
