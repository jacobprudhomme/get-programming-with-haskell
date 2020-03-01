#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1..5]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: [Char]
letters = ['a', 'b', 'c']

aPet :: [Char]
aPet = "cat"

anotherPet :: String
anotherPet = "dog"

ageAndHeight :: (Int,Int)
ageAndHeight = (22, 175)

firstLastMiddle :: (String,String,Char)
firstLastMiddle = ("Jacob", "Prud'homme", 'F')

streetAddress :: (Int,String)
streetAddress = (620, "Pennycross Lane")

double :: Int -> Int
double n = n * 2

half :: Int -> Double
half n = (fromIntegral n) / 2

halve :: Int -> Int
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show $ n * 2

number = read "6" :: Double

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int,String,String)
makeAddress number street town = (number, street, town)

makeAddress' =
  (\number ->
    (\street ->
      (\town ->
        (number, street, town))))

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n =
  if even n
  then f n
  else n

simpleInt :: Int -> Int
simpleInt n = n

simpleChar :: Char -> Char
simpleChar c = c

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x, y, z)

nameTriple = makeTriple "Oscar" 'D' "Grouch"

-- filter :: (a -> Bool) -> [a] -> [a]

tail' :: [a] -> [a]
tail' []     = []
tail' (x:xs) = xs

-- head' :: [a] -> a
-- head' []     = ???
-- head' (x:xs) = x

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init []     = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x
