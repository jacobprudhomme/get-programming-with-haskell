#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20


example1 = (*) <$> Just 6 <*> Just 7
example2 = div <$> Just 6 <*> Just 7
example3 = mod <$> Just 6 <*> Just 7

qc1 = (++) <$> Just "abc" <*> Just "def"

pureExample1 :: Maybe Int
pureExample1 = pure 6

pureExample2 = pure (6 +) <*> Just 5
-- This is equivalent to (6 +) <$> Just 5

qc2 :: IO String
qc2 = pure "Hello, world!"

data Blah a b = Blah a b

data Box a = Box a

data ResourceConstrained a = NoResources | Okay a

-- pure (+) <*> (1,2) <*> (3,4) != (1+3,1+4,2+3,2+4) == (4,5,5,6)
-- This is because the type of (1,2) and (3,4) are not the same as (4,5,5,6)

additionInContext = pure (+) <*> [1000, 2000, 3000] <*> [500, 20000]

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [500, 20000]

-- This won't work because it is trying to force a deterministic calculation
-- totalPrize :: Int
-- totalPrize = (+) doorPrize boxPrize

totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize

multBoxPrize :: [Int]
multBoxPrize = [10, 50]

totalPrize' :: [Int]
totalPrize' = pure (*) <*> doorPrize <*> multBoxPrize

allCombos = pure (*) <*> [2..4] <*> [2..4]

-- Calculates way more composite numbers than we need, but it works!
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoToN
  where
    twoToN = [2..n]
    composite = pure (*) <*> twoToN <*> twoToN
    isNotComposite = not . (`elem` composite)

data User = User
  { name    :: String
  , gamerId :: Int
  , score   :: Int
  } deriving Show

testNames :: [String]
testNames =
  [ "John Smith"
  , "Robert'); DROP TABLE Students;--"
  , "Christina NULL"
  , "Randall Munroe"
  , "Jacob Prud'homme" -- Adding my name took it from 36 combos to 45
  ]

testIds :: [Int]
testIds = [1337, 0123, 999999]

testScores :: [Int]
testScores = [0, 100000, -99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f a = pure f <*> a

test1 = allFmap (+ 1) [1, 2, 3] == [2, 3, 4]
test2 = allFmap (+ 1) (Just 5) == Just 6
test3 = allFmap (+ 1) Nothing == Nothing

example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> pure ((+) 2 4) <*> pure 6

originalNumBeers = [6, 12]
numBeersConsumedAlready = [4]
numFriendsComing = [2, 3]
numBeersExpConsumed = [3, 4]

howMuchBeerToBuy = negate $ minimum $ pure (-)
  <*> (pure (-)
    <*> originalNumBeers
    <*> numBeersConsumedAlready)
  <*> (pure (*)
    <*> numFriendsComing
    <*> numBeersExpConsumed)
