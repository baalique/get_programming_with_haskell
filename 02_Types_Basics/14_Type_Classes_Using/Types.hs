data SixSidedDice = S1 | S2 | S3 | S4 | S5 | S6

instance Show SixSidedDice where
    show S1 = "I"
    show S2 = "II"
    show S3 = "III"
    show S4 = "IV"
    show S5 = "V"
    show S6 = "VI"

instance Eq SixSidedDice where
    (==) S1 S1 = True
    (==) S2 S2 = True
    (==) S3 S3 = True
    (==) S4 S4 = True
    (==) S5 S5 = True
    (==) S6 S6 = True
    (==) _  _  = False

instance Ord SixSidedDice where
    compare S1 S1 = EQ
    compare S1 _  = LT
    compare _  S1 = GT
    compare S2 S2 = EQ
    compare S2 _  = LT
    compare _  S2 = GT
    compare S3 S3 = EQ
    compare S3 _  = LT
    compare _  S3 = GT
    compare S4 S4 = EQ
    compare S4 _  = LT
    compare _  S4 = GT
    compare S5 S5 = EQ
    compare S5 _  = LT
    compare _  S5 = GT
    compare S6 S6 = EQ
