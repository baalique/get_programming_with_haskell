minOfThree :: Ord a => a -> a -> a -> a
minOfThree x y z = minimum [x, y, z]

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

mainMin :: IO ()
mainMin = do
    minInt <- minOfInts
    print minInt


data User = User
    { name  :: String
    , uid   :: Int
    , score :: Int
    }
    deriving Show

mainUser :: IO ()
mainUser = do
    user <- User <$> getLine <*> readInt <*> readInt
    print user
