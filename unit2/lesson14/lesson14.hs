#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

import Data.List


data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum, Eq, Ord, Show)

data SixSidedDie' = S1' | S2' | S3' | S4' | S5' | S6'

instance Show SixSidedDie' where
--show :: SixSidedDie' -> String
  show S1' = "one"
  show S2' = "two"
  show S3' = "three"
  show S4' = "four"
  show S5' = "five"
  show S6' = "six"

data SixSidedDie'' = S1'' | S2'' | S3'' | S4'' | S5'' | S6'' deriving (Enum)

instance Show SixSidedDie'' where
  show S1'' = "I"
  show S2'' = "II"
  show S3'' = "III"
  show S4'' = "IV"
  show S5'' = "V"
  show S6'' = "VI"

data TwoSidedDie = One | Two

--show :: TwoSidedDie -> String
--show One = "one"
--show Two = "two"

instance Eq SixSidedDie' where
  (==) S1' S1' = True
  (==) S2' S2' = True
  (==) S3' S3' = True
  (==) S4' S4' = True
  (==) S5' S5' = True
  (==) S6' S6' = True
  (==) _ _   = False

-- properFraction is RealFrac's minimal complete definition

instance Ord SixSidedDie' where
  compare S6' S6' = EQ
  compare S6' _   = GT
  compare _ S6'   = LT
  compare S5' S5' = EQ
  compare S5' _   = GT
  compare _ S5'   = LT
  compare S4' S4' = EQ
  compare S4' _   = GT
  compare _ S4'   = LT

data Test1 = AA | ZZ deriving (Eq, Ord)
data Test2 = ZZZ | AAA deriving (Eq, Ord)

instance Enum SixSidedDie' where
  toEnum 0 = S1'
  toEnum 1 = S2'
  toEnum 2 = S3'
  toEnum 3 = S4'
  toEnum 4 = S5'
  toEnum 5 = S6'
  toEnum _ = error "No such value"

  fromEnum S1' = 0
  fromEnum S2' = 1
  fromEnum S3' = 2
  fromEnum S4' = 3
  fromEnum S5' = 4
  fromEnum S6' = 5

dieSides = [S1' .. S6']
evenDieSides = [S2', S4' .. S6']
bigDieSides = [S4' .. S6']
--errorDieSides = [S1' ..]
notErrorDieSides = [S1 ..]

type Name = (String,String)

names :: [Name]
names =
  [("Emil", "Cioran")
  ,("Eugene", "Thacker")
  ,("Friedrich", "Nietzsche")
  ]

sortedNames = sort names

--instance Ord Name where
--  compare (fn1, ln1) (fn2, ln2) = compare (ln1, fn1) (ln2, fn2)

--data Name' = Name' (String,String) deriving (Eq, Show) is equivalent to:
newtype Name' = Name' (String,String) deriving (Eq, Show)

instance Ord Name' where
  compare (Name' (fn1, ln1)) (Name' (fn2, ln2)) =
    compare (ln1, fn1) (ln2, fn2)

names' :: [Name']
names' = map Name' names

sortedNames' = sort names'

instance Eq SixSidedDie'' where
  (==) d1 d2 = fromEnum d1 == fromEnum d2

instance Ord SixSidedDie'' where
  compare d1 d2 = compare (fromEnum d1) (fromEnum d2)

data FiveSidedDie = FSD1 | FSD2 | FSD3 | FSD4 | FSD5 deriving (Enum, Eq, Ord)

class (Enum a, Eq a, Ord a) => Die a where
  oddRoll :: a -> Bool
  evenRoll :: a -> Bool

  evenRoll = not . oddRoll

instance Die FiveSidedDie where
  oddRoll d = even $ fromEnum d
