askForName :: IO ()
askForName = putStrLn "Your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >> getLine >>= (\name -> return $ nameStatement name) >>= putStrLn
