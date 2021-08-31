import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           System.Environment             ( getArgs )
import           System.IO                      ( )
import           System.Random                  ( randomRIO )

getBytesAndCharacters :: IO ()
getBytesAndCharacters = do
    args <- getArgs
    let fileName = head args
    fileData <- B.readFile fileName
    let bytesSize        = B.length fileData
    let charactersLength = T.length . E.decodeUtf8 $ fileData
    putStrLn $ mconcat ["Bytes: ", show bytesSize, "; characters: ", show charactersLength]


reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, change, after]
  where
    (before, rest ) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    change          = BC.reverse target

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return $ reverseSection start sectionSize bytes
