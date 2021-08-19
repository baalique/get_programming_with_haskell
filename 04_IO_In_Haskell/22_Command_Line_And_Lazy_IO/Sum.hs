import           Control.Monad                  ( replicateM )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    let linesLen = if not $ null args then read $ head args else 0
    numbers <- replicateM linesLen getLine
    let ints = map read numbers :: [Int]
    print $ sum ints

customReplicateM :: Monad m => m a -> Int -> m [a]
customReplicateM f n = mapM (const f) [0 .. n - 1]
