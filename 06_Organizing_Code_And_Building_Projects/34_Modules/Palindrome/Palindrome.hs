module Palindrome
    ( isPalindrome
    ) where

import           Data.Char                      ( isPunctuation
                                                , isSpace
                                                , toLower
                                                )

stripWhitespaces :: String -> String
stripWhitespaces = filter (not . isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . isPunctuation)

toLowerCase :: String -> String
toLowerCase = map toLower

preprocess :: String -> String
preprocess = stripWhitespaces . stripPunctuation . toLowerCase

isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText where cleanText = preprocess text
