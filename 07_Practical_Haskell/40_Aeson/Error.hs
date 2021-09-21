{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , Value(Object)
                                                , decode
                                                , encode
                                                , object
                                                )
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T

data ErrorMessage = ErrorMessage
    { message   :: T.Text
    , errorCode :: Int
    }
    deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"
    parseJSON _          = undefined

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) = object ["message" .= message, "error" .= errorCode]

sampleError :: BC.ByteString
sampleError = "{\"message\":\"Some Error!\",\"error\": 123}"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "OK" 0

encodedError :: BC.ByteString
encodedError = encode anErrorMessage
