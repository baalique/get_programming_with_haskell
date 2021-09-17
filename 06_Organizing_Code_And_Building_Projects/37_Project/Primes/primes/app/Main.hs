module Main where

import           Primes                         ( isPrime
                                                , primeFactors
                                                )

main :: IO ()
main = do
    putStrLn "Is prime:"
    n <- getLine
    print $ isPrime $ read n
    putStrLn "Prime factors:"
    x <- getLine
    print $ primeFactors $ read n
