import           Data.Char                      ( isPunctuation
                                                , toLower
                                                , toUpper
                                                )
import qualified Data.Text                     as T
import           Lib                            ( isPalindrome
                                                , preprocess
                                                )
import           Test.QuickCheck                ( Args(maxSuccess)
                                                , quickCheckWith
                                                , stdArgs
                                                )
import           Test.QuickCheck.Instances      ( )

prop_punctuationInvariant :: T.Text -> Bool
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant :: T.Text -> Bool
prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

prop_whitespacesInvariant :: T.Text -> Bool
prop_whitespacesInvariant text = preprocess text == preprocess noWhitespacesText
    where noWhitespacesText = T.filter (/= ' ') text

prop_lowerCaseInvariant :: T.Text -> Bool
prop_lowerCaseInvariant text = preprocess text == preprocess lowerCaseText where lowerCaseText = T.map toLower text

prop_upperCaseInvariant :: T.Text -> Bool
prop_upperCaseInvariant text = preprocess text == preprocess upperCaseText where upperCaseText = T.map toUpper text

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_reverseInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_whitespacesInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_lowerCaseInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_upperCaseInvariant
    putStrLn "Done"
