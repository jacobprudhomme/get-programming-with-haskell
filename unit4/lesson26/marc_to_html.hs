#!/usr/bin/env stack
-- stack exec ghci --resolver lts-14.20

{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO


type Author = T.Text
type Title = T.Text
data Book = Book
  { author :: Author
  , title  :: Title
  } deriving Show

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", (title book), "</strong>\n"]
    authorInTags = mconcat ["<em>", (author book), "</em>\n"]

book1 :: Book
book1 = Book
  { title = "The Conspiracy Against the Human Race"
  , author = "Ligotti, Thomas"
  }

book2 :: Book
book2 = Book
  { title = "A Short History of Decay"
  , author = "Cioran, Emil"
  }

book3 :: Book
book3 = Book
  { title = "The Tears of Eros"
  , author = "Bataille, Georges"
  }

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat
  [ "<html>\n"
  , "<head>\n"
  , "<title>Books</title>\n"
  , "<meta charset='utf-8'/>\n"
  , "</head>\n"
  , "<body>\n"
  , booksHtml
  , "</body>\n"
  , "</html>"
  ]
  where booksHtml = mconcat $ map bookToHtml books

myBooks :: [Book]
myBooks = [book1, book2, book3]

main' :: IO ()
main' = TIO.writeFile "some_books.html" $ booksToHtml myBooks

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

leaderLen :: Int
leaderLen = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLen record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . TE.decodeUtf8

getRecordLen :: MarcLeaderRaw -> Int
getRecordLen leader = rawToInt $ B.take 5 leader

nextRecordAndRest :: B.ByteString -> (MarcRecordRaw,B.ByteString)
nextRecordAndRest marcStream = B.splitAt recordLen marcStream
  where recordLen = getRecordLen marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream =
  if marcStream == B.empty
  then []
  else next : allRecords rest
  where (next, rest) = nextRecordAndRest marcStream

main'' :: IO ()
main'' = do
  marcData <- B.readFile "sample.mrc"
  let marcRecords = allRecords marcData
  print $ length marcRecords

type MarcDirectoryRaw = B.ByteString

getDirectoryBaseAddr :: MarcLeaderRaw -> Int
getDirectoryBaseAddr leader = rawToInt baseAddr
  where baseAddr = B.take 5 $ B.drop 12 leader

getDirectoryLen :: MarcLeaderRaw -> Int
getDirectoryLen leader =
  getDirectoryBaseAddr leader - (leaderLen + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLen afterLeader
  where
    directoryLen = getDirectoryLen record
    afterLeader = B.drop leaderLen record

type MarcDirectoryEntryRaw = B.ByteString

directoryEntryLen :: Int
directoryEntryLen = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if directory == B.empty
  then []
  else next : splitDirectory rest
  where (next, rest) = B.splitAt directoryEntryLen directory

data FieldMetadata = FieldMetadata
  { tag        :: T.Text
  , fieldLen   :: Int
  , fieldStart :: Int
  } deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag len start
  where
    (theTag, rest) = B.splitAt 3 entry
    textTag = TE.decodeUtf8 theTag
    (rawLen, rawStart) = B.splitAt 4 rest
    len = rawToInt rawLen
    start = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = TE.decodeUtf8 byteStringVal
  where
    recordLen = getRecordLen record
    baseAddr = getDirectoryBaseAddr record
    baseRecord = B.drop baseAddr record
    entryBase = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringVal = B.take (fieldLen fieldMetadata) entryBase

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if length results < 1
  then Nothing
  else Just $ head results
  where
    metadata = getFieldMetadata $ splitDirectory $ getDirectory record
    results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _                          = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
  if results == []
  then Nothing
  else Just $ T.drop 1 $ head results
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfield) . T.head) subfields

lookupVal :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupVal aTag subfield record =
  lookupSubfield entryMetadata subfield record
    where entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupVal titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupVal authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title,Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title,Maybe Author)] -> [Book]
pairsToBooks pairs =
  map (\(title, author) -> Book
    { title = fromJust title
    , author = fromJust author
    })
    justPairs
      where
        justPairs =
          filter (\(title, author) ->
            isJust title && isJust author)
            pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . (take n) . marcToPairs

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed
