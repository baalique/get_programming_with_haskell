import Data.Char (toLower)

elem' val xs = length filtered >= 1
  where
    filtered = filter (== val) xs

isPalindrome str = processedStr == reverse processedStr
  where
    processedStr = filter (/= ' ') lowerStr
    lowerStr = map toLower str

harmonic n = foldl (+) 0 (map (1 /) [2 .. n])
