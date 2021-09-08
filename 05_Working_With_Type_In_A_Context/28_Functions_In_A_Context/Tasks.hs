import qualified Data.Map                      as M

type LatLong = (Double, Double)

haversineIO :: (LatLong -> LatLong -> Double) -> IO LatLong -> IO LatLong -> IO Double
haversineIO f coords1 coords2 = do
    c1 <- coords1
    c2 <- coords2
    let dist = f c1 c2
    return dist

haversineIO' :: (LatLong -> LatLong -> Double) -> IO LatLong -> IO LatLong -> IO Double
haversineIO' f coords1 coords2 = f <$> coords1 <*> coords2


data RobotPart = RobotPart
    { name        :: String
    , description :: String
    , cost        :: Double
    , count       :: Int
    }
    deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name = "left arm", description = "robot's left arm", cost = 1000.00, count = 3 }

rightArm :: RobotPart
rightArm = RobotPart { name = "right arm", description = "robot's right arm", cost = 1025.00, count = 5 }

robotHead :: RobotPart
robotHead = RobotPart { name = "robot head", description = "robot's head", cost = 5092.25, count = 2 }

partsDB :: M.Map Int RobotPart
partsDB = M.fromList $ zip [1, 2, 3] [leftArm, rightArm, robotHead]

printCost :: Maybe Double -> IO ()
printCost Nothing  = print "No data"
printCost (Just x) = print x

main :: IO ()
main = do
    firstInput  <- getLine
    secondInput <- getLine
    let part1 = M.lookup (read firstInput) partsDB
    let part2 = M.lookup (read secondInput) partsDB
    let p     = min <$> (cost <$> part1) <*> (cost <$> part2)
    printCost p
