module Main where

import qualified Palindrome

main :: IO ()
main = do
    putStrLn "Enter a word"
    text <- getLine
    let response = if Palindrome.isPalindrome text then "Yes" else "No"
    putStrLn response
