#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import Data.List
import Data.Semigroup


last' :: [a] -> a
last' = head . reverse

min' :: Ord a => [a] -> a
min' = head . sort

max' :: Ord a => [a] -> a
max' = last' . sort

all' :: (a -> Bool) -> [a] -> Bool
all' pred = (foldr (&&) True) . (map pred)

any' :: (a -> Bool) -> [a] -> Bool
any' pred = (foldr (||) False) . (map pred)

{-
instance Semigroup Integer where
  (<>) x y = x + y
-}
instance Semigroup Integer where
  (<>) x y = x * y

data Colour
  = Red
  | Yellow
  | Blue
  | Green
  | Purple
  | Orange
  | Brown
  deriving (Eq, Show)

instance Semigroup Colour where
  (<>) Red Yellow  = Orange
  (<>) Yellow Red  = Orange
  (<>) Red Blue    = Purple
  (<>) Blue Red    = Purple
  (<>) Blue Yellow = Green
  (<>) Yellow Blue = Green
  (<>) a b         =
    if a == b then a else Brown

notAssociative = ((Green <> Blue) <> Yellow) /= (Green <> (Blue <> Yellow))

howMuch :: Int -> String
howMuch n
  | n > 10    = "A whole bunch"
  | n > 0     = "Not much"
  | otherwise = "We're in debt!"

data Colour'
  = NoColour
  | Red'
  | Yellow'
  | Blue'
  | Green'
  | Purple'
  | Orange'
  | Brown'
  deriving (Eq, Show)

instance Semigroup Colour' where
  (<>) NoColour b = b
  (<>) a NoColour = a
  (<>) a b
    | a == b                                       = a
    | all (`elem` [Red', Blue', Purple']) [a, b]   = Purple'
    | all (`elem` [Blue', Yellow', Green']) [a, b] = Green'
    | all (`elem` [Red', Yellow', Orange']) [a, b] = Orange'
    | otherwise                                    = Brown'

associative = ((Green' <> Blue') <> Yellow') == (Green' <> (Blue' <> Yellow'))

sameThing1 = ([1,2,3] ++ []) == ([1,2,3] <> [])
sameThing2 = ([1,2,3] <> []) == ([1,2,3] `mappend` mempty)

instance Monoid Integer where
  mempty = 1

mconcatTest = mconcat ["Does ", "this", " make ", "sense?"]

type Events = [String]
type Probs = [Double]
data ProbTable = ProbTable Events Probs

createProbTable :: Events -> Probs -> ProbTable
createProbTable events probs = ProbTable events normalizedProbs
  where
    probsTotal = sum probs
    normalizedProbs = map (/ probsTotal) probs

showProbTableRow :: String -> Double -> String
showProbTableRow event prob = mconcat [event, " | ", show prob, "\n"]

instance Show ProbTable where
  show (ProbTable events probs) = mconcat rows
    where rows = zipWith showProbTableRow events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = zipWith f newL1 cycledL2
  where
    n = length l2
    repeatedL1 = map (replicate n) l1
    newL1 = mconcat repeatedL1
    cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner x y = mconcat [x, "-", y]

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup ProbTable where
  (<>) ptable1 (ProbTable [] []) = ptable1
  (<>) (ProbTable [] []) ptable2 = ptable2
  (<>) (ProbTable e1 p1) (ProbTable e2 p2) =
    createProbTable newEvents newProbs
    where
      newEvents = combineEvents e1 e2
      newProbs = combineProbs p1 p2

instance Monoid ProbTable where
  mempty = ProbTable [] []

coin :: ProbTable
coin = createProbTable ["heads", "tails"] [0.5, 0.5]

spinner :: ProbTable
spinner = createProbTable ["red", "green", "blue"] [0.1, 0.2, 0.7]

instance Monoid Colour' where
  mempty = NoColour

newtype Events' = Events' [String]
newtype Probs' = Probs' [Double]
data ProbTable' = ProbTable' Events' Probs'

combineEvents' :: Events' -> Events' -> Events'
combineEvents' (Events' e1) (Events' e2) = Events' $ cartCombine combiner e1 e2
  where combiner x y = mconcat [x, "-", y]

combineProbs' :: Probs' -> Probs' -> Probs'
combineProbs' (Probs' p1) (Probs' p2) = Probs' $ cartCombine (*) p1 p2

instance Semigroup Events' where
  (<>) e1 (Events' []) = e1
  (<>) (Events' []) e2 = e2
  (<>) e1 e2 = combineEvents' e1 e2

instance Monoid Events' where
  mempty = Events' []

instance Semigroup Probs' where
  (<>) p1 (Probs' []) = p1
  (<>) (Probs' []) p2 = p2
  (<>) p1 p2 = combineProbs' p1 p2

instance Monoid Probs' where
  mempty = Probs' []

instance Semigroup ProbTable' where
  (<>) ptable1 (ProbTable' (Events' []) (Probs' [])) = ptable1
  (<>) (ProbTable' (Events' []) (Probs' [])) ptable2 = ptable2
  (<>) (ProbTable' e1 p1) (ProbTable' e2 p2) =
    createProbTable' newEvents newProbs
    where
      newEvents = combineEvents' e1 e2
      newProbs = combineProbs' p1 p2

instance Monoid ProbTable' where
  mempty = ProbTable' (Events' []) (Probs' [])

instance Show ProbTable' where
  show (ProbTable' (Events' events) (Probs' probs)) = mconcat rows
    where rows = zipWith showProbTableRow events probs

createProbTable' :: Events' -> Probs' -> ProbTable'
createProbTable' events (Probs' probs) =
  ProbTable' events (Probs' normalizedProbs)
  where
    probsTotal = sum probs
    normalizedProbs = map (/ probsTotal) probs

coin' :: ProbTable'
coin' = createProbTable' (Events' ["heads", "tails"]) (Probs' [0.5, 0.5])

spinner' :: ProbTable'
spinner' = createProbTable' (Events' ["red", "green", "blue"]) (Probs' [0.1, 0.2, 0.7])
