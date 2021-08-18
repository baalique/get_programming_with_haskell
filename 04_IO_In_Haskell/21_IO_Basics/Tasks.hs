import qualified Data.Map                      as M

getHello :: [Char] -> [Char]
getHello str = "Hello, " ++ str ++ "!"

names :: M.Map Int [Char]
names = M.fromList [(1, "Alice"), (2, "Bob"), (3, "Jane")]

sayHello :: Int -> Maybe [Char]
sayHello nameId = do
    name <- M.lookup nameId names
    return name


fib :: Int -> Integer
fib = (fibs !!)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibIO :: IO ()
fibIO = do
    n <- getLine
    let fibN = fib $ read n
    putStrLn $ show fibN
