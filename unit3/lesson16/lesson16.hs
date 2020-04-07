#!/usr/bin/env stack
-- stack ghci --resolver lts-14.20

data AuthorName = AuthorName String String

data Book = Book AuthorName String String Int Double

data AuthorName' = AuthorName'
  { firstName :: String
  , lastName  :: String
  }

data Book' = Book'
  { author :: AuthorName'
  , isbn   :: String
  , title  :: String
  , year   :: Int
  , price' :: Double
  }

data Car = Car
data Spoiler = Spoiler
data SportsCar = SportsCar Car Spoiler

data Bool' = False' | True'

type FirstName = String
type MiddleName = String
type LastName = String
data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithFirst FirstName Char Char
  | TwoInitialsWithLast Char Char LastName
  deriving (Show)

data Author = Author Name deriving (Show)
data Artist
  = ArtistPerson Name
  | ArtistBand String
  deriving (Show)
data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  deriving (Show)

hpLovecraft :: Creator
hpLovecraft = AuthorCreator $ Author $ TwoInitialsWithLast 'H' 'P' "Lovecraft"

data Book'' = Book''
  { author'   :: Creator
  , isbn'     :: String
  , bookTitle :: String
  , bookYear  :: Int
  , bookPrice :: Double
  }

data VinylRecord = VinylRecord
  { artist      :: Creator
  , recordTitle :: String
  , recordYear  :: Int
  , recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name        :: String
  , toyDescription :: String
  , toyPrice    :: Double
  }

data Number = Number Int Int Int Int
data Address = Address Int String String
data ContactInfo = ContactInfo Name Number Address

data Pamphlet = Pamphlet
  { pamphletTitle       :: String
  , pamphletDescription :: String
  , contactInfo         :: ContactInfo
  }

data StoreItem
  = BookItem Book''
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price _ = 0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show $ author' book
madeBy (RecordItem record) = show $ artist record
madeBy _ = "Unknown"

data Circle = Circle { radius :: Double }
data Square = Square { side :: Double }
data Rectangle = Rectangle { height :: Double, width :: Double }
data Shape
  = CircleShape Circle
  | SquareShape Square
  | RectShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape circle) = 2 * pi * (radius circle)
perimeter (SquareShape square) = 4 * (side square)
perimeter (RectShape rectangle) =
  2 * (height rectangle) + 2 * (width rectangle)

area :: Shape -> Double
area (CircleShape circle) = pi * (radius circle)^2
area (SquareShape square) = (side square)^2
area (RectShape rectangle) =
  (height rectangle) * (width rectangle)
