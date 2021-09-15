module Palindrome
    ( isPalindrome
    ) where

import           Data.Char                      ( isPunctuation
                                                , isSpace
                                                , toLower
                                                )
import qualified Data.Text                     as T

stripWhitespaces :: T.Text -> T.Text
stripWhitespaces = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

toLowerCase :: T.Text -> T.Text
toLowerCase = T.map toLower

preprocess :: T.Text -> T.Text
preprocess = stripWhitespaces . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText where cleanText = preprocess text
