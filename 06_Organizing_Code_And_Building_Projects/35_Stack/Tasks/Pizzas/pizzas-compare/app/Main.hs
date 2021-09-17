module Main where

import           Calculations                   ( comparePizzas )
import           Lib                            ( describePizza )

main :: IO ()
main = do
    putStrLn "First size:"
    size1 <- getLine
    putStrLn "First cost:"
    cost1 <- getLine
    putStrLn "Second size:"
    size2 <- getLine
    putStrLn "Second cost:"
    cost2 <- getLine
    let pizza1      = (read size1, read cost1)
    let pizza2      = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)
