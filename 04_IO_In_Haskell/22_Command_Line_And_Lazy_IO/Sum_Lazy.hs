import           Data.List.Split                ( splitOn )

mainReversed :: IO ()
mainReversed = do
    userInput <- getContents
    let reversed = reverse userInput
    putStrLn reversed

mainSum :: IO ()
mainSum = do
    userInput <- getContents
    let numbers = map read $ splitOn " " userInput
    print $ sum numbers

(-+^) :: Num a => a -> a -> a
(-+^) a b = a + b ^ 2
infix 5 -+^

mainSquares :: IO ()
mainSquares = do
    userInput <- getContents
    let numbers = map read $ splitOn " " userInput
    print $ foldl (-+^) 0 numbers

main :: IO ()
main = mainSquares
