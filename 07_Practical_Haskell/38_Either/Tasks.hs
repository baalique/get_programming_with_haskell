import           Data.Char                      ( isDigit )

data AddError = FirstNumberError | SecondNumberError | BothNumbersError deriving Show

addStrsInt :: String -> String -> Either AddError Int
addStrsInt x y | not (all isDigit x) || not (all isDigit y) = Left BothNumbersError
               | not (all isDigit x) = Left FirstNumberError
               | not (all isDigit y) = Left SecondNumberError
               | otherwise           = Right $ read x + read y

succSafe :: Int -> Either String Int
succSafe n | n == maxBound = Left "Too large number"
           | otherwise     = Right $ n + 1

tailSafe :: [a] -> [a]
tailSafe []       = []
tailSafe (_ : xs) = xs

maxLastN :: Int
maxLastN = 1000

safeLast :: [a] -> Either a String
safeLast = safeLast' maxLastN

safeLast' :: Int -> [a] -> Either a String
safeLast' n []       = Right "Empty list"
safeLast' 0 _        = Right "Too large list"
safeLast' _ [x     ] = Left x
safeLast' n (x : xs) = safeLast' (n - 1) xs
