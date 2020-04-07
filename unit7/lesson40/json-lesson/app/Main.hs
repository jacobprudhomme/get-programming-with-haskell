module Main where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Data.Text as T
import Data.Text.IO as TIO
import GHC.Generics


data Book = Book
  { title  :: T.Text
  , author :: T.Text
  , year   :: Int
  } deriving (Generic, Show)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book
  { author = "Will Kurt"
  , title  = "Learn Haskell"
  , year   = 2017
  }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Cioran\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

bookFromWrongJSON' :: Either String Book
bookFromWrongJSON' = eitherDecode wrongJSON

data Name = Name
  { firstName :: T.Text
  , lastName  :: T.Text
  } deriving (Generic, Show)

instance FromJSON Name
instance ToJSON Name

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\":123}"

data ErrorMessage = ErrorMessage
  { message   :: T.Text
--, error     :: Int doesn't work because error is already defined
  , errorCode :: Int  -- Can't automatically derive generic
  } deriving Show

instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                 <*> v .: "error"

exampleMessage :: Maybe T.Text
exampleMessage = Just "Oops"

exampleError :: Maybe Int
exampleError = Just 123

exampleErrorMessage :: Maybe ErrorMessage
exampleErrorMessage = ErrorMessage
  <$> exampleMessage
  <*> exampleError

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where
  toJSON (ErrorMessage msg errCode) =
    object [ "message" .= msg
           , "error" .= errCode
           ]

anErrorMsg :: ErrorMessage
anErrorMsg = ErrorMessage "Everything is ok" 0

errorMsgJSON :: BC.ByteString
errorMsgJSON = encode anErrorMsg

data Name' = Name'
  { firstName' :: T.Text
  , lastName'  :: T.Text
  } deriving Show

instance FromJSON Name' where
  parseJSON (Object v) =
    Name' <$> v .: "firstName"
          <*> v .: "lastName"

instance ToJSON Name' where
  toJSON (Name' first last) =
    object [ "firstName" .= first
           , "lastName" .= last
           ]

data NOAAResult = NOAAResult
  { uid          :: T.Text
  , minDate      :: T.Text
  , maxDate      :: T.Text
  , name         :: T.Text
  , dataCoverage :: Float
  , resultId     :: T.Text
  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"

data ResultSet = ResultSet
  { offset :: Int
  , count  :: Int
  , limit  :: Int
  } deriving (Generic, Show)

instance FromJSON ResultSet

data Metadata = Metadata { resultSet :: ResultSet } deriving (Generic, Show)

instance FromJSON Metadata where
  parseJSON (Object v) = Metadata <$> v .: "resultset"

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results  :: [NOAAResult]
  } deriving (Generic, Show)

instance FromJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing        = TIO.putStrLn "Error loading data"
printResults (Just results) = forM_ results (TIO.putStrLn . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults

instance ToJSON NOAAResult where
  toJSON noaaResult = object
    [ "uid" .= uid noaaResult
    , "mindate" .= minDate noaaResult
    , "maxdate" .= maxDate noaaResult
    , "name" .= name noaaResult
    , "datacoverage" .= dataCoverage noaaResult
    , "id" .= resultId noaaResult
    ]

instance ToJSON ResultSet

instance ToJSON Metadata where
  toJSON (Metadata resultSet) =
    object [ "resultset" .= resultSet ]

instance ToJSON NOAAResponse

exampleNOAAResponse :: NOAAResponse
exampleNOAAResponse = NOAAResponse
  (Metadata (ResultSet 0 1 1))
  [NOAAResult "1" "1" "1" "1" 1 "1"]

exampleNOAAResponseJSON :: BC.ByteString
exampleNOAAResponseJSON = encode exampleNOAAResponse

data IntList
  = Cons Int IntList
  | EmptyList
  deriving (Generic, Show)

instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList
