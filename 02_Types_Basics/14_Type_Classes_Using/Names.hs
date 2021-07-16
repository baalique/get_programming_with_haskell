type FirstName = String
type LastName = String

newtype Name = Name (FirstName, LastName) deriving (Show, Eq)

instance Ord Name where
    compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)


names :: [Name]
names = [Name ("John", "Doe"), Name ("Jane", "Doe"), Name ("Alice", "White"), Name ("Bob", "Black")]
