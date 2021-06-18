main :: IO ()
main = do
  putStrLn "Letter recipient?"
  recipient <- getLine
  putStrLn "Book title?"
  title <- getLine
  putStrLn "Letter author?"
  author <- getLine
  putStrLn (createEmail recipient title author)

toPart recipient = "Dear " ++ recipient ++ "!\n"

bodyPart bookTitle = "Thanks for buying \"" ++ bookTitle ++ "\"!\n"

fromPart author = "With respect,\n" ++ author

createEmail recepient bookTitle author =
  toPart recepient
    ++ bodyPart bookTitle
    ++ fromPart author
