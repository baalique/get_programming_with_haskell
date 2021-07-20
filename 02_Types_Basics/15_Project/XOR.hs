module XOR where

xorBool :: Bool -> Bool -> Bool
xorBool x y = (x || y) && not (x && y)

xorPair :: (Bool, Bool) -> Bool
xorPair (x, y) = xorBool x y

xor :: [Bool] -> [Bool] -> [Bool]
xor xs ys = map xorPair (zip xs ys)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainder == 0 then False : intToBits' nextVal else True : intToBits' nextVal
  where
    remainder = n `mod` 2
    nextVal   = n `div` 2

maxBits :: Int
maxBits = length $ intToBits' maxBound

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
  where
    reversedBits  = reverse $ intToBits' n
    missingBits   = maxBits - length reversedBits
    leadingFalses = replicate missingBits False

charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2 ^ snd x) trueLocations)
  where
    size          = length bits
    indices       = [size - 1, size - 2 .. 0]
    trueLocations = filter fst (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInt

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plainText = map (\p -> fst p `xor` snd p) (zip padBits plainTextBits)
  where
    padBits       = map charToBits pad
    plainTextBits = map charToBits plainText

applyOTP :: String -> String -> String
applyOTP pad plainText = map bitsToChar (applyOTP' pad plainText)

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad
