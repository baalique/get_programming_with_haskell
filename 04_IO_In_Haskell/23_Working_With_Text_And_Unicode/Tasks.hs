{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TIOL

getHello :: T.Text -> T.Text
getHello str = "Hello, " <> str <> "!"

sayHello :: IO ()
sayHello = do
    putStrLn "Type your name:"
    name <- TIO.getLine
    let hello = getHello name
    TIO.putStrLn hello

toInts :: TL.Text -> [Int]
toInts = map (read . TL.unpack) . TL.lines

sumMain :: IO ()
sumMain = do
    input <- TIOL.getContents
    let numbers = toInts input
    TIOL.putStrLn $ (TL.pack . show . sum) numbers

main :: IO ()
main = sumMain
