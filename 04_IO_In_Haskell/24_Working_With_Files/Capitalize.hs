import           Data.Char                      ( toUpper )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Environment             ( getArgs )

capitalize :: T.Text -> [Char]
capitalize = map toUpper . T.unpack

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    fileData <- TIO.readFile fileName
    _        <- writeFile fileName $ capitalize fileData
    return ()
