import qualified Data.Map                      as M

type Html = String

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

renderHtml :: RobotPart -> Html
renderHtml part = mconcat
  [ "<h2>"
  , partName
  , "</h2>"
  , "<p><h3>desc</h3>"
  , partDesc
  , "</p><p><h3>cost</h3>"
  , partCost
  , "</p><p><h3>count</h3>"
  , partCount
  , "</p>"
  ]
 where
  partName  = name part
  partDesc  = description part
  partCost  = show (cost part)
  partCount = show (count part)

partsDB :: M.Map Int RobotPart
partsDB = M.fromList $ zip [1, 2, 3] [leftArm, rightArm, robotHead]

partVal :: Maybe RobotPart
partVal = M.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> M.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: M.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

printCost :: Maybe Double -> IO ()
printCost Nothing  = putStrLn "Not found"
printCost (Just x) = print x

main :: IO ()
main = do
  n <- getLine
  let part = M.lookup (read n) partsDB
  printCost (cost <$> part)
