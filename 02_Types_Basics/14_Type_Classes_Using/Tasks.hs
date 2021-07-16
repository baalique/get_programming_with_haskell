data Number = One | Two | Three deriving Enum

instance Eq Number where
    (==) num1 num2 = fromEnum num1 == fromEnum num2

instance Ord Number where
    compare num1 num2 = compare (fromEnum num1) (fromEnum num2)


data FiveSidedDice = F1 | F2 | F3 | F4 | F5 deriving (Enum, Eq)

class (Eq a, Enum a) => Dice a where
    roll :: Int -> a

instance Dice FiveSidedDice where
    roll n = toEnum (n `mod` 5)

instance Show FiveSidedDice where
    show F1 = "I"
    show F2 = "II"
    show F3 = "III"
    show F4 = "IV"
    show F5 = "V"
