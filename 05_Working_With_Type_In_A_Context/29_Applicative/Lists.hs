primesToN :: Integer -> [Integer]
primesToN n = filter isPrime xs
 where
  xs        = [2 .. n]
  composite = (*) <$> xs <*> xs
  isPrime   = not . (`elem` composite)


data User = User
  { name  :: String
  , uid   :: Int
  , score :: Int
  }
  deriving Show

names :: [[Char]]
names = ["John", "Alice", "Bob", "Jane"]

ids :: [Int]
ids = [1, 4, 8, 11, 24]

scores :: [Int]
scores = [1000, 500, 2500]

testData :: [User]
testData = User <$> names <*> ids <*> scores
