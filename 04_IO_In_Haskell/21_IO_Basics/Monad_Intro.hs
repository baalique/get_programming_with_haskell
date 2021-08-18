import qualified Data.Map                      as M

type Cost = Double
type Size = Double
type Area = Double
type Pizza = (Size, Cost)

areaGivenParameter :: Size -> Area
areaGivenParameter size = pi * (size / 2) ^ 2

costPerCm :: (Size, Cost) -> Double
costPerCm (size, cost) = cost / areaGivenParameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2 then p1 else p2
 where
  costP1 = costPerCm p1
  costP2 = costPerCm p2

describePizza :: (Size, Cost) -> [Char]
describePizza (size, cost) = "Pizza with size " ++ show size ++ " is cheaper by " ++ show costSqCm ++ " per cm^2"
  where costSqCm = costPerCm (size, cost)

pizzaIO :: IO ()
pizzaIO = do
  putStrLn "First size:"
  size1 <- getLine
  putStrLn "First cost:"
  cost1 <- getLine
  putStrLn "Second size:"
  size2 <- getLine
  putStrLn "Second cost:"
  cost2 <- getLine
  let pizza1      = (read size1, read cost1)
  let pizza2      = (read size2, read cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  putStrLn (describePizza betterPizza)

costData :: M.Map Int Cost
costData = M.fromList [(1, 150), (2, 220)]

sizeData :: M.Map Int Size
sizeData = M.fromList [(1, 30), (2, 50)]

pizzaMaybe :: Maybe [Char]
pizzaMaybe = do
  size1 <- M.lookup 1 sizeData
  cost1 <- M.lookup 1 costData
  size2 <- M.lookup 2 sizeData
  cost2 <- M.lookup 2 costData
  let pizza1      = (size1, cost1)
  let pizza2      = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
