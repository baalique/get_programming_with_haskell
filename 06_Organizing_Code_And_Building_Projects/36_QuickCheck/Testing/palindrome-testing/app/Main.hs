module Main where

import qualified Data.Text.IO                  as TIO
import           Lib                            ( isPalindrome )

main :: IO ()
main = do
    TIO.putStrLn "Enter a word"
    text <- TIO.getLine
    let response = if isPalindrome text then "Yes" else "No"
    TIO.putStrLn response
