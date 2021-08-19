import           Data.Char                      ( isAlpha )
import           Text.Regex.TDFA                ( (=~) )

calcRegex :: [Char]
calcRegex = "^ *([0-9]*) *(\\+|-) *([0-9]*) *$"

calc :: String -> Either String Int
calc str = res
  where
    expr = case str =~ calcRegex of
        [] -> Nothing
        x  -> Just $ tail $ head x
    res = case expr of
        Nothing          -> Left "Wrong expression"
        Just [x, sgn, y] -> Right $ evl (read x :: Int) sgn (read y :: Int)
        _                -> Left "Wrong expression"
    evl x "+" y = x + y
    evl x "-" y = x - y
    evl _ _   _ = undefined

simpleCalc :: IO ()
simpleCalc = do
    input <- getContents
    let res = calc input
    print res


quotes :: [[Char]]
quotes = map (("Quote " ++) . show) [1 .. 15]

getQuote :: [[Char]] -> [[Char]]
getQuote [] = []
getQuote (x : xs) | all isAlpha x && n >= 1 && n <= length quotes = quote : getQuote xs
  where
    quote = quotes !! (read x - 1)
    n     = read x
getQuote _ = []

mainQuotes :: IO ()
mainQuotes = do
    input <- getContents
    mapM_ putStrLn $ getQuote $ lines input

main :: IO ()
main = mainQuotes
