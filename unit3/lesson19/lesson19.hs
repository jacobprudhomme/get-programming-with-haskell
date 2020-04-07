#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import Data.List
import Data.Maybe

import qualified Data.Map as Map


data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Eq, Show)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

found = Map.lookup 13 organCatalog -- :: Maybe Organ

notFound = Map.lookup 6 organCatalog -- :: Maybe Organ

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents drawers catalog = map getContents drawers
  where
    getContents drawer = Map.lookup drawer catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available =
  length $ filter (\x -> x == Just organ) available

numBrains = countOrgan Brain availableOrgans

numHearts = countOrgan Heart availableOrgans

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething _       = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing      = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing  = 0
numOrZero (Just x) = x

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
  show (Vat organ)    = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ)    = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a)    = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a)    = (Kitchen, Bag a)

process :: Organ -> (Location,Container)
process organ = placeInLocation $ organToContainer organ

report :: (Location,Container) -> String
report (location, container) =
  show container ++
  " in the " ++
  show location

{-
processRequest :: Int -> Map.Map Int Organ -> String
processRequest drawer catalog = report $ process organ
  where organ = Map.lookup drawer catalog
-}

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report $ process organ
processAndReport _            = "Error, drawer not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest drawer catalog = processAndReport organ
  where organ = Map.lookup drawer catalog

report' :: Maybe (Location,Container) -> String
report' Nothing                      = "Container not found"
report' (Just (location, container)) =
  show container ++
  " in the " ++
  show location

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers available = length $ filter isNothing available

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing  = Nothing
maybeMap f (Just x) = Just (f x)
