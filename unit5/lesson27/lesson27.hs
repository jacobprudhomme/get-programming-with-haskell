#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import System.Environment

import qualified Data.Map as Map


successfulReq :: Maybe Int
successfulReq = Just 6

failedReq :: Maybe Int
failedReq = Nothing

incMaybe :: Maybe Int -> Maybe Int
incMaybe Nothing  = Nothing
incMaybe (Just n) = Just $ n + 1

incOnSuccess = incMaybe successfulReq

incOnFailure = incMaybe failedReq

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing  = Nothing
reverseMaybe (Just s) = Just $ reverse s

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
  fmap f Nothing'  = Nothing'
  fmap f (Just' x) = Just' $ f x

incOnSuccess' = fmap (+ 1) successfulReq

incOnFailure' = (+ 1) <$> failedReq

successStr :: Maybe String
successStr = show <$> successfulReq

failureStr :: Maybe String
failureStr = fmap show failedReq

reverseStr :: Maybe String -> Maybe String
reverseStr = fmap reverse

data RobotPart = RobotPart
  { name :: String
  , description :: String
  , cost :: Double
  , count :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart
  { name = "Left Arm"
  , description = "Left arm for doing a big grab"
  , cost = 1000.00
  , count = 3
  }

rightArm :: RobotPart
rightArm = RobotPart
  { name = "Right Arm"
  , description = "Right arm for doing a big hit"
  , cost = 1025.00
  , count = 5
  }

robotHead :: RobotPart
robotHead = RobotPart
  { name = "Robot Head"
  , description = "Robot head for making angry faces"
  , cost = 5092.99
  , count = 2
  }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat
  [ "<h2>", partName, "</h2>\n"
  , "<p><h3>desc</h3>", partDesc, "</p>\n"
  , "<p><h3>cost</h3>", partCost, "</p>\n"
  , "<p><h3>count</h3>", partCount, "</p>"
  ]
  where
    partName = name part
    partDesc = description part
    partCost = show $ cost part
    partCount = show $ count part

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
  where
    keys = [1..3]
    vals = [leftArm, rightArm, robotHead]
    keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = map snd $ Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

allPartsHtml' :: [Html]
allPartsHtml' = map renderHtml allParts

allParts' :: [RobotPart]
allParts' = snd <$> Map.toList partsDB

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

{-
  A view of all these functions together:

  partHtml :: Maybe Html
  partHtml = renderHtml <$> partVal

  allPartsHtml :: [Html]
  allPartsHtml = renderHtml <$> allParts

  htmlPartsDB :: Map.Map Int Html
  htmlPartsDB = renderHtml <$> partsDB

  htmlSnippet :: IO Html
  htmlSnippet = renderHtml <$> leftArmIO
-}

data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box x) = Box $ f x

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap (replicate n)

myBox :: Box Int
myBox =  Box 1

wrap = Box
wrapped = fmap wrap myBox

unwrap (Box x) = x
unwrapped = fmap unwrap wrapped

costOfPart id = cost <$> Map.lookup id partsDB

main :: IO ()
main = do
  args <- getArgs
  let id = read $ head args :: Int
  print $ costOfPart id
