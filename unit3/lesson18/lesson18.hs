#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

import Data.Char

import qualified Data.Map as Map


data Box a = Box a deriving Show

n = 6 :: Int
boxedInt = Box n

word = "box"
boxedWord = Box word

fn x = x
boxedFn = Box fn

boxedBox = Box boxedInt

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x

wrappedBoxedChar :: Box (Box Char)
wrappedBoxedChar = wrap $ Box 'c'

data Triple a = Triple a a a deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 53.2 12.7

type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Jacob" "Fox" "Prud'homme"

type Initials = Triple Char

initials :: Initials
initials = Triple 'V' 'F' 'D'

first :: Triple a -> a
first (Triple x _ _) = x

second :: Triple a -> a
second (Triple _ y _) = y

third :: Triple a -> a
third (Triple _ _ z) = z

toList' :: Triple a -> [a]
toList' (Triple x y z) = [x,y,z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

newPoint = transform (* 3) aPoint

reversedName = transform reverse aPerson

lowerInitials = transform toLower initials

lowerInitialsString = toList' lowerInitials

data List a = Empty | Cons a (List a) deriving Show

builtinEx1 :: [Int]
builtinEx1 = 1:2:3:[]

ourEx1 :: List Int
ourEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'c':'a':'t':[]

ourEx2 :: List Char
ourEx2 = Cons 'c' (Cons 'a' (Cons 't' Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty       = Empty
ourMap f (Cons x xs) = Cons (f x) (ourMap f xs)

mapOnOurEx1 = ourMap (* 2) ourEx1

item1Count :: (String,Int)
item1Count = ("Erasers", 25)

item2Count :: (String,Int)
item2Count = ("Pencils", 25)

item3Count :: (String,Int)
item3Count = ("Pens", 13)

item4Count :: (String,Double)
item4Count = ("Paper", 12.4)

itemInventory :: [(String,Int)]
itemInventory = [item1Count,item2Count,item3Count]
-- itemInventory = [item1Count,item2Count,item3Count,item4Count] would be an error

data Organ
  = Heart
  | Brain
  | Kidney
  | Spleen
  deriving (Enum, Eq, Ord, Show)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

drawerIds :: [Int]
drawerIds = [2, 7, 13, 14, 21, 24]

drawerOrganPairs :: [(Int,Organ)]
drawerOrganPairs = zip drawerIds organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList drawerOrganPairs

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

organCountPairs :: [(Organ,Int)]
organCountPairs = foldr foldOrganCountPairs [] allOrgans
  where
    allOrgans = [Heart ..]
    countOrgans curr = length $ filter (== curr) organs
    organCountPair curr = (curr, countOrgans curr)
    foldOrganCountPairs curr acc = organCountPair curr : acc

organInventory :: Map.Map Organ Int
organInventory = Map.fromList organCountPairs
