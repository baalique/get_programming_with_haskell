askForName :: IO ()
askForName = putStrLn "Your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return $ nameStatement name) >>= putStrLn

helloNameDo :: IO ()
helloNameDo = do
    askForName
    name <- getLine
    putStrLn $ nameStatement name

helloWithoutDo :: IO ()
helloWithoutDo = getLine >>= (\name -> (\statement -> putStrLn statement) (nameStatement name))

helloWithDo :: IO ()
helloWithDo = do
    name <- getLine
    let statement = nameStatement name
    putStrLn statement

echo :: IO ()
echo = getLine >>= putStrLn

echoDo :: IO ()
echoDo = do
    s <- getLine
    putStrLn s
