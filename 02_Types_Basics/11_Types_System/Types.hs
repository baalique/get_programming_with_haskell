average' :: [Int] -> Double
average' aList = fromIntegral (sum aList) / fromIntegral (length aList)

half :: Int -> Int
half x = x `div` 2

printDouble :: Int -> String
printDouble x = show (x * 2)

makeAddress number street town = (number, street, town)

makeAddressLambda = (\number -> (\street -> (\town -> (number, street, town))))

simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple a b c = (a, b, c)

foldl' :: (t -> a -> t) -> t -> [a] -> t
foldl' f init []       = init
foldl' f init (x : xs) = foldl' f newInit xs where newInit = f init x
