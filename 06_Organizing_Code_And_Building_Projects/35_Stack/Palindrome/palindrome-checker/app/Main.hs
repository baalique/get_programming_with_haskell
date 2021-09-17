module Main where

import           Data.Text.IO                  as TIO
                                                ( getLine
                                                , putStrLn
                                                )
import           Palindrome                     ( isPalindrome )

main :: IO ()
main = do
    TIO.putStrLn "Enter a word"
    text <- TIO.getLine
    let response = if isPalindrome text then "Yes" else "No"
    TIO.putStrLn response
