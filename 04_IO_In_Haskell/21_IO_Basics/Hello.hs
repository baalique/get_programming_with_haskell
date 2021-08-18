getHello :: [Char] -> [Char]
getHello str = "Hello, " ++ str ++ "!"

sayHello :: IO ()
sayHello = do
    putStrLn "Type your name:"
    name <- getLine
    let hello = getHello name
    putStrLn hello
