{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString               as B
import           Data.Function                  ( on )
import           Data.List                      ( sort
                                                , sortBy
                                                )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.IO                  as TIO
import           System.Environment             ( getArgs )

type Author = T.Text
type Title = T.Text
type HTML = T.Text

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString

type FieldText = T.Text

data Book = Book
    { author :: Author
    , title  :: Title
    }
    deriving Show

data FieldMetadata = FieldMetadata
    { tag         :: T.Text
    , fieldLength :: Int
    , fieldStart  :: Int
    }
    deriving Show

bookToHtml :: Book -> HTML
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]
  where
    titleInTags  = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> HTML
booksToHtml books = mconcat
    [ "<html>\n"
    , "<head><title>Books</title>"
    , "<meta charset=’utf-8’/>"
    , "</head>\n"
    , "<body>\n"
    , booksHtml
    , "\n</body>\n"
    , "</html>"
    ]
    where booksHtml = (mconcat . map bookToHtml . sortBy (compare `on` title)) books

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt (getRecordLength marcStream) marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty then [] else next : allRecords rest
    where (next, rest) = nextAndRest marcStream

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt $ B.take 5 $ B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength record
    afterLeader     = B.drop leaderLength record

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty then [] else nextEntry : splitDirectory restEntries
    where (nextEntry, restEntries) = B.splitAt dirEntryLength directory

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
  where
    (theTag, rest)        = B.splitAt 3 entry
    textTag               = E.decodeUtf8 theTag
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength             = rawToInt rawLength
    theStart              = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    recordLength    = getRecordLength record
    baseAddress     = getBaseAddress record
    baseRecord      = B.drop baseAddress record
    baseAtEntry     = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

fieldDelimeter :: Char
fieldDelimeter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if null results then Nothing else Just (head results)
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results  = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing              subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record = if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField  = getTextField record fieldMetadata
    subFields = T.split (== fieldDelimeter) rawField
    results   = filter ((== subfield) . T.head) subFields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
    where entryMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles  = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book { title = fromJust title, author = fromJust author }) justPairs
    where justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> HTML
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

main :: IO ()
main = do
    args     <- getArgs
    marcData <- B.readFile $ head args
    let processed = processRecords (read $ args !! 1) marcData
    TIO.writeFile (args !! 2) processed
