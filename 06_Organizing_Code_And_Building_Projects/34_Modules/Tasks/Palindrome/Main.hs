module Main where

import qualified Data.Text                     as T
import qualified Palindrome

main :: IO ()
main = do
    putStrLn "Enter a word"
    text <- getLine
    let response = if Palindrome.isPalindrome (T.pack text) then "Yes" else "No"
    putStrLn response
