{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , decode
                                                , eitherDecode
                                                , encode
                                                )
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

data Book = Book
    { title  :: T.Text
    , author :: T.Text
    , year   :: Int
    }
    deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book { author = "John Doe", title = "Book One", year = 2021 }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

encodedBook :: BC.ByteString
encodedBook = "{\"year\":2021,\"author\":\"John Doe\",\"title\":\"Book One\"}"

myBook' :: Maybe Book
myBook' = decode encodedBook

myBook'' :: Either String Book
myBook'' = eitherDecode encodedBook
