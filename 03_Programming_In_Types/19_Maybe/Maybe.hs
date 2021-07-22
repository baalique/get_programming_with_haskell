import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map

data Part = CPU | RAM | HDD | Motherboard deriving Eq

instance Show Part where
    show CPU         = "CPU"
    show RAM         = "RAM"
    show HDD         = "HDD"
    show Motherboard = "Motherboard"

parts :: [Part]
parts = [CPU, CPU, RAM, RAM, HDD, Motherboard]

ids :: [Int]
ids = [2, 7, 12, 14, 21, 24]

partsPairs :: [(Int, Part)]
partsPairs = zip ids parts

partsCatalog :: Map.Map Int Part
partsCatalog = Map.fromList partsPairs

possibleBoxes :: [Int]
possibleBoxes = [1 .. 50]

getBoxContents :: [Int] -> Map.Map Int Part -> [Maybe Part]
getBoxContents ids catalog = map getContents ids where getContents = (`Map.lookup` catalog)

availableParts :: [Maybe Part]
availableParts = getBoxContents possibleBoxes partsCatalog

countPart :: Part -> [Maybe Part] -> Int
countPart part available = length $ filter (\x -> x == Just part) available

isSomething :: Maybe Part -> Bool
isSomething Nothing  = False
isSomething (Just _) = True

justTheParts :: [Maybe Part]
justTheParts = filter isSomething availableParts

showPart :: Maybe Part -> String
showPart Nothing     = ""
showPart (Just part) = show part

partList :: [String]
partList = map showPart justTheParts

cleanList :: String
cleanList = intercalate ", " partList

numOrZero :: Maybe Int -> Int
numOrZero Nothing  = 0
numOrZero (Just x) = x

data Box = Small Part | Middle Part | Large Part

instance Show Box where
    show (Small  part) = show part ++ " in little box"
    show (Middle part) = show part ++ " in middle box"
    show (Large  part) = show part ++ " in large box"

data Location = MainRoom | Kitchen | Bathroom

instance Show Location where
    show MainRoom = "Main room"
    show Kitchen  = "Kitchen"
    show Bathroom = "Bathroom"

partToBox :: Part -> Box
partToBox CPU         = Small CPU
partToBox RAM         = Small RAM
partToBox HDD         = Middle HDD
partToBox Motherboard = Large Motherboard

placeInLocation :: Box -> (Location, Box)
placeInLocation (Small  x) = (Bathroom, Small x)
placeInLocation (Middle x) = (Kitchen, Middle x)
placeInLocation (Large  x) = (MainRoom, Large x)

process :: Part -> (Location, Box)
process = placeInLocation . partToBox

report :: (Location, Box) -> String
report (location, box) = show box ++ " (place: " ++ show location ++ ")"

processAndReport :: Maybe Part -> String
processAndReport Nothing     = "Error, no such id was founded"
processAndReport (Just part) = report $ process part

processRequest :: Int -> Map.Map Int Part -> String
processRequest id catalog = processAndReport $ Map.lookup id catalog

report' :: Maybe (Location, Box) -> String
report' Nothing                = "Error, no such box was founded"
report' (Just (location, box)) = show box ++ " (place: " ++ show location ++ ")"

emptyBoxes :: [Maybe Part] -> Int
emptyBoxes []              = 0
emptyBoxes (Nothing  : xs) = 1 + emptyBoxes xs
emptyBoxes ((Just _) : xs) = emptyBoxes xs

----------------------------------------------------------------

maybeMap :: (b -> a) -> [Maybe b] -> [Maybe a]
maybeMap f []              = []
maybeMap f (Nothing  : xs) = Nothing : maybeMap f xs
maybeMap f ((Just x) : xs) = Just (f x) : maybeMap f xs
