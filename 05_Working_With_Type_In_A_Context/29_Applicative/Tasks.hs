allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f a = (pure f) <*> a


example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> pure 2 <*> pure 4) <*> pure 6


start :: [Int]
start = [6, 12]

remaining :: [Int]
remaining = (\c -> c - 4) <$> start

guests :: [Int]
guests = [2, 3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

numberPerGuest :: [Int]
numberPerGuest = [3, 4]

totalNeeded :: [Int]
totalNeeded = (*) <$> numberPerGuest <*> totalPeople

toPurchase :: [Int]
toPurchase = (-) <$> totalNeeded <*> remaining
