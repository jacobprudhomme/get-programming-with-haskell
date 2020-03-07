#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import qualified Data.Map as Map


data User = User
  { name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show

samePlayer = User {name = "Sue", gamerId = 1337, score = 9001}
samePlayer' = User "Sue" 1337 9001

serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

maybeUser :: Maybe User
maybeUser = User <$> serverUsername <*> serverGamerId <*> serverScore

readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
  putStrLn "Enter a username, gamer ID, and score:"
  user <- User <$> getLine <*> readInt <*> readInt
  print user

noUser :: Maybe User
noUser = User <$> Nothing <*> Just 1234 <*> Just 5678

type LatLong = (Double,Double)

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

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO loc1 loc2 = do
  unwrappedLoc1 <- loc1
  unwrappedLoc2 <- loc2
  return $ haversine unwrappedLoc1 unwrappedLoc2

haversineIO' :: IO LatLong -> IO LatLong -> IO Double
haversineIO' loc1 loc2 = haversine <$> loc1 <*> loc2

data RobotPart = RobotPart
  { rName :: String
  , desc :: String
  , cost :: Double
  , count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
  { rName = "Left Arm"
  , desc = "Left arm for doing a big grab"
  , cost = 1000.00
  , count = 3
  }

rightArm :: RobotPart
rightArm = RobotPart
  { rName = "Right Arm"
  , desc = "Right arm for doing a big hit"
  , cost = 1025.00
  , count = 5
  }

leftLeg :: RobotPart
leftLeg = RobotPart
  { rName = "Left Leg"
  , desc = "Left leg for doing Chuck Norris kicks"
  , cost = 800.99
  , count = 20
  }

rightLeg :: RobotPart
rightLeg = RobotPart
  { rName = "Right Leg"
  , desc = "Right leg for doing Bruce Lee kicks"
  , cost = 1234.56
  , count = 1
  }

robotHead :: RobotPart
robotHead = RobotPart
  { rName = "Robot Head"
  , desc = "Robot head for making angry faces"
  , cost = 5092.99
  , count = 2
  }

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList
  [ (1, leftArm)
  , (2, rightArm)
  , (3, leftLeg)
  , (4, rightLeg)
  , (5, robotHead)
  ]

printPartCost :: Maybe RobotPart -> IO ()
printPartCost Nothing     = putStrLn "Error: a part with that ID does not exist"
printPartCost (Just part) = putStrLn $ show part ++ " is cheaper at $" ++ show (cost part)

cheapestPart :: RobotPart -> RobotPart -> RobotPart
cheapestPart part1 part2 = if part1Cost < part2Cost then part1 else part2
  where
    part1Cost = cost part1
    part2Cost = cost part2

main' :: IO ()
main' = do
  putStrLn "Enter two part ID's:"
  part1ID <- readInt
  part2ID <- readInt
  let part1 = Map.lookup part1ID partsDB
  let part2 = Map.lookup part2ID partsDB
  printPartCost $ cheapestPart <$> part1 <*> part2
