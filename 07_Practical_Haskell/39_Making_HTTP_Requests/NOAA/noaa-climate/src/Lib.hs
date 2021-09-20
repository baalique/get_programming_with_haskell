module Lib where

import           APISettings                    ( apiPath
                                                , getToken
                                                , noaaHost
                                                )
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Char8    as LC
import           Network.HTTP.Simple            ( Request
                                                , Response
                                                , defaultRequest
                                                , getResponseBody
                                                , getResponseStatus
                                                , getResponseStatusCode
                                                , setRequestHeader
                                                , setRequestHost
                                                , setRequestMethod
                                                , setRequestPath
                                                , setRequestPort
                                                , setRequestSecure
                                                )
import           Network.HTTP.Types.Status      ( Status(statusMessage) )

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
    setRequestMethod method
        $ setRequestHost host
        $ setRequestHeader "token" [token]
        $ setRequestPath path
        $ setRequestSecure True
        $ setRequestPort 443 defaultRequest

buildRequestNoSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNoSSL token host method path =
    setRequestMethod method
        $ setRequestHost host
        $ setRequestHeader "token" [token]
        $ setRequestPath path
        $ setRequestPort 80 defaultRequest

apiRequest :: IO Request
apiRequest = do
    token <- getToken
    let req = buildRequest token noaaHost "GET" apiPath
    return req

handleResponse :: Response LC.ByteString -> [Char] -> IO ()
handleResponse response filename = do
    let status = getResponseStatusCode response
    if status == 200
        then saveResponseToFile response filename
        else
            let message      = statusMessage $ getResponseStatus response
                errorMessage = concat ["Error ", show status, ". Message: ", show message]
            in  putStrLn errorMessage

saveResponseToFile :: Response LC.ByteString -> [Char] -> IO ()
saveResponseToFile response filename = do
    putStrLn $ "Results was saved to " ++ filename
    let jsonBody = getResponseBody response
    L.writeFile filename jsonBody
