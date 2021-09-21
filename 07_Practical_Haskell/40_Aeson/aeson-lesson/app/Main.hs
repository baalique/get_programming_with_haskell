{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                  ( forM_ )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value(Object)
                                                , decode
                                                , object
                                                )
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )

data NOAAResult = NOAAResult
    { uid          :: T.Text
    , mindate      :: T.Text
    , maxdate      :: T.Text
    , name         :: T.Text
    , datacoverage :: Double
    , resultId     :: T.Text
    }
    deriving Show

data Resultset = Resultset
    { offset :: Int
    , count  :: Int
    , limit  :: Int
    }
    deriving (Show, Generic)

newtype Metadata = Metadata { resultset :: Resultset} deriving (Show,Generic)

data NOAAResponse = NOAAResponse
    { metadata :: Metadata
    , results  :: [NOAAResult]
    }
    deriving (Show, Generic)

instance FromJSON NOAAResponse

instance FromJSON NOAAResult where
    parseJSON (Object v) =
        NOAAResult
            <$> v
            .:  "uid"
            <*> v
            .:  "mindate"
            <*> v
            .:  "maxdate"
            <*> v
            .:  "name"
            <*> v
            .:  "datacoverage"
            <*> v
            .:  "id"
    parseJSON _ = error "Invalid JSON"

instance FromJSON Resultset

instance FromJSON Metadata

instance ToJSON NOAAResult where
    toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) = object
        [ "uid" .= uid
        , "mindate" .= mindate
        , "maxdate" .= maxdate
        , "name" .= name
        , "datacoverage" .= datacoverage
        , "id" .= resultId
        ]

instance ToJSON Resultset

instance ToJSON Metadata

instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing        = putStrLn "Load data error"
printResults (Just results) = forM_ results (print . name)

main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults  = results <$> noaaResponse
    printResults noaaResults
