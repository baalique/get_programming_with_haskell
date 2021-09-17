module Palindrome where

import qualified Data.Text                     as T
import           Lib                            ( preprocess )

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText where cleanText = preprocess text
