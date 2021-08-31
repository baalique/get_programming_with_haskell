import           System.IO

mainHello :: IO ()
mainHello = do
    helloFile <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine helloFile
    putStrLn firstLine
    secondLine  <- hGetLine helloFile
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile secondLine
    hClose goodbyeFile
    putStrLn "Done"

mainLineExists :: IO ()
mainLineExists = do
    helloFile <- openFile "empty.txt" ReadMode
    hasLine   <- hIsEOF helloFile
    firstLine <- if not hasLine then hGetLine helloFile else return "Empty file"
    putStrLn "Done"
