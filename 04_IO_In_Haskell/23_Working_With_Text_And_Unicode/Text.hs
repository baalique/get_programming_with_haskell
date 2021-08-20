{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where
    pieces      = T.splitOn query fullText
    highlighted = mconcat ["{", query, "}"]

query :: T.Text
query = "or"

fullText :: T.Text
fullText = "Hello world or not hello world"

main :: IO ()
main = do
    TIO.putStrLn (highlight query fullText)
