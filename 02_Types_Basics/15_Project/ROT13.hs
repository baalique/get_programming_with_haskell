module ROT13 where

data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
 where
  halfAlphabet = alphabetSize `div` 2
  offset       = fromEnum c + halfAlphabet
  rotation     = offset `mod` alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar = rotN (largestCharNumber + 1)

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map rot4l
 where
  alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
  rot4l     = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
 where
  halfN    = n `div` 2
  offset   = if even n then fromEnum c + halfN else 1 + fromEnum c + halfN
  rotation = offset `mod` n

rotEncoder :: String -> String
rotEncoder = map rotChar
 where
  alphaSize = 1 + fromEnum (maxBound :: Char)
  rotChar   = rotN alphaSize

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
 where
  alphaSize      = 1 + fromEnum (maxBound :: Char)
  rotCharDecoder = rotNdecoder alphaSize
