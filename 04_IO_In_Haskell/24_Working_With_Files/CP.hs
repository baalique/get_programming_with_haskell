import           System.Environment             ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    let firstName  = head args
    let secondName = args !! 1
    fileData <- readFile firstName
    _        <- writeFile secondName fileData
    return ()
