data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
    show TooLarge     = "Too large value"
    show InvalidValue = "Cannot be checked"

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (x : _) = Just x

primes :: [Int]
primes = [2, 3, 5, 7, 11, 13, 17, 19]

maxN :: Int
maxN = 20

isPrimeMaybe :: Int -> Maybe Bool
isPrimeMaybe n | n < 2     = Nothing
               | n > maxN  = Nothing
               | otherwise = Just $ n `elem` primes

headEither :: [a] -> Either String a
headEither []      = Left "Empty list"
headEither (x : _) = Right x

isPrimeEither :: Int -> Either String Bool
isPrimeEither n | n < 2     = Left "Cannot be checked"
                | n > maxN  = Left "Too large value"
                | otherwise = Right (n `elem` primes)

isPrimeEitherError :: Int -> Either PrimeError Bool
isPrimeEitherError n | n < 2     = Left InvalidValue
                     | n > maxN  = Left TooLarge
                     | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True      ) = "Prime"
displayResult (Right False     ) = "Composite"
displayResult (Left  primeError) = show primeError

main :: IO ()
main = do
    putStrLn "Number:"
    n <- read <$> getLine
    let res = isPrimeEitherError n
    putStrLn $ displayResult res
