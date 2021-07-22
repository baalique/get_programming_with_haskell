import qualified Data.Map                      as Map

data Part = CPU | RAM | HDD | Motherboard deriving (Show, Eq, Ord)

parts :: [Part]
parts = [CPU, CPU, RAM, RAM, HDD, Motherboard]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

partsPairs :: [(Int, Part)]
partsPairs = zip ids parts

partsCatalog :: Map.Map Int Part
partsCatalog = Map.fromList partsPairs

countParts :: [Part] -> [(Part, Int)]
countParts ps = countParts' ps []
  where
    countParts' []       acc = acc
    countParts' (p : ps) acc = countParts' ps (addPart p acc)
    addPart p acc = addToCounter p acc

addToCounter :: Eq a => a -> [(a, Int)] -> [(a, Int)]
addToCounter p xs = addToCounter' p xs []
  where
    addToCounter' p [] acc                     = (p, 1) : acc
    addToCounter' p ((x, c) : xs) acc | x == p = (x, c + 1) : xs ++ acc
    addToCounter' p (x : xs) acc               = addToCounter' p xs (x : acc)

partsInventory :: Map.Map Part Int
partsInventory = Map.fromList (countParts parts)
