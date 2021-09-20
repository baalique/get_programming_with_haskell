module Main where

import           APISettings                    ( outputFileName )
import qualified Lib
import           Network.HTTP.Simple            ( httpLBS )

main :: IO ()
main = do
    req      <- Lib.apiRequest
    response <- httpLBS req
    Lib.handleResponse response outputFileName
