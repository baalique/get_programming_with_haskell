import qualified Data.Map                      as Map

data Part = CPU | RAM | HDD | Motherboard deriving (Show, Eq)

parts :: [Part]
parts = [CPU, CPU, RAM, RAM, HDD, Motherboard]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

partsPairs :: [(Int, Part)]
partsPairs = zip ids parts

partsCatalog :: Map.Map Int Part
partsCatalog = Map.fromList partsPairs

