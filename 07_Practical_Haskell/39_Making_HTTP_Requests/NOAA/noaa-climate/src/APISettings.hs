module APISettings where

import qualified Data.ByteString.Char8         as BC

getToken :: IO BC.ByteString
getToken = BC.pack <$> readFile ".env"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

outputFileName :: [Char]
outputFileName = "data.json"
