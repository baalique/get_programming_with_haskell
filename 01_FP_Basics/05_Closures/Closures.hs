ifEven func x = if even x then func x else x

genIfEvenX x = \f -> ifEven f x

--

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = \apiKey resource id -> getRequestUrl host apiKey resource id

exampleUrlBuilder = genHostRequestBuilder "https://example.com"

genApiRequestBuilder hostBuilder apiKey = \resource id -> hostBuilder apiKey resource id

exampleUrlApiBuilder = genApiRequestBuilder exampleUrlBuilder "APIKEY"

genApiRequestBuilderResource hostBuilder resource apiKey = \id -> hostBuilder resource apiKey id
