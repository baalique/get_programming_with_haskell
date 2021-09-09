import           Control.Monad                  ( guard )
import           Data.Char                      ( toUpper )

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    x <- [1 .. n]
    return $ 2 ^ x

powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (2 ^) [1 .. n]

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    x <- [1 .. n]
    let powersOfTwo   = 2 ^ x
    let powersOfThree = 3 ^ x
    return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    even <- [2, 4 .. n]
    odd  <- [1, 3 .. n]
    return (even, odd)

valAndSquare :: [(Int, Int)]
valAndSquare = do
    x <- [1 .. 10]
    return (x, x ^ 2)

evensGuard :: Int -> [Int]
evensGuard n = do
    x <- [1 .. n]
    guard $ even x
    return x

filterGuard :: (a -> Bool) -> [a] -> [a]
filterGuard pr xs = do
    x <- xs
    guard $ pr x
    return x

evenSquares :: Int -> [Int]
evenSquares n = do
    x <- [0 .. n]
    let squared = x ^ 2
    guard $ even squared
    return squared

powersOfTwoCompr :: Int -> [Int]
powersOfTwoCompr n = [ 2 ^ x | x <- [1 .. n] ]

powersOfTwoAndThreeCompr :: Int -> [(Int, Int)]
powersOfTwoAndThreeCompr n = [ (p2, p3) | x <- [1 .. n], let p2 = 2 ^ x, let p3 = 3 ^ x ]

allEvenOddsCompr :: Int -> [(Int, Int)]
allEvenOddsCompr n = [ (evenValue, oddValue) | evenValue <- [2, 4 .. n], oddValue <- [1, 3 .. n] ]

colorsCompr :: [[Char]]
colorsCompr =
    [ s | color <- ["Brown", "Blue", "Pink", "Orange"], let s = "Mr. " ++ (toUpper (head color) : tail color) ]
