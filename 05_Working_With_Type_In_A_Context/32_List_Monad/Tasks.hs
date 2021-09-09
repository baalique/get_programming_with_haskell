import qualified Data.Map                      as M

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Eq, Ord, Enum, Show)

daysInMonth :: M.Map Month Int
daysInMonth = M.fromList
    [ (January  , 31)
    , (February , 28)
    , (March    , 31)
    , (April    , 30)
    , (May      , 31)
    , (June     , 30)
    , (July     , 31)
    , (August   , 31)
    , (September, 30)
    , (October  , 31)
    , (November , 30)
    , (December , 31)
    ]

datesCompr :: [[Char]]
datesCompr =
    [ date | month <- [January ..], day <- [1 .. daysInMonth M.! month], let date = show month ++ " " ++ show day ]

datesDo :: [[Char]]
datesDo = do
    month <- [January ..]
    day   <- [1 .. daysInMonth M.! month]
    let date = show month ++ " " ++ show day
    return date

datesMonad :: [[Char]]
datesMonad =
    (\months -> months >>= (\month -> [1 .. daysInMonth M.! month] >>= (\day -> return $ show month ++ " " ++ show day))
        )
        [January ..]
