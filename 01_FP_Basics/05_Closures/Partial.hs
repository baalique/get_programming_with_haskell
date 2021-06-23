getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

exampleUrlBuilder = getRequestUrl "https://example.com"

exampleBuilder = getRequestUrl "https://example.com" "APIKEY" "book"

-- 

flipBinaryArgs func = \x y -> func y x

subtract2 x = flipBinaryArgs (-) 2 x
