{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           System.Environment             ( getArgs )
import           System.IO                      ( IOMode(ReadMode)
                                                , hClose
                                                , hGetContents
                                                , openFile
                                                )

getCounts :: [Char] -> (Int, Int, Int)
getCounts str = (charCount, wordCount, lineCount)
  where
    charCount = length str
    wordCount = (length . words) str
    lineCount = (length . lines) str

countsToStr :: (Int, Int, Int) -> [Char]
countsToStr (c, w, l) = concat ["Symbols: ", show c, "; words: ", show w, "; lines: ", show l]

mainBroken :: IO ()
mainBroken = do
    args <- getArgs
    let fileName = head args
    input <- readFile fileName
    let summary = (countsToStr . getCounts) input
    appendFile "stats.txt" (concat [fileName, " ", summary, "\n"])
    putStrLn summary

mainLazy :: IO ()
mainLazy = do
    args <- getArgs
    let fileName = head args
    file  <- openFile fileName ReadMode
    input <- hGetContents file
    let summary = (countsToStr . getCounts) input
    putStrLn summary
    hClose file
    appendFile "stats.txt" (concat [fileName, " ", summary, "\n"])


getTextCounts :: T.Text -> (Int, Int, Int)
getTextCounts str = (charCount, wordCount, lineCount)
  where
    charCount = T.length str
    wordCount = (length . T.words) str
    lineCount = (length . T.lines) str

countsToText :: (Int, Int, Int) -> T.Text
countsToText (c, w, l) = T.pack $ concat ["Symbols: ", show c, "; words: ", show w, "; lines: ", show l]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TIO.readFile fileName
    let summary = (countsToText . getTextCounts) input
    TIO.appendFile "stats.txt" (mconcat [T.pack fileName, " ", summary, "\n"])
    TIO.putStrLn summary
