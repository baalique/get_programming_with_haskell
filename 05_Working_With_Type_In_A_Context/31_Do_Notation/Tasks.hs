import qualified Data.Map                      as M

type Cost = Double
type Size = Double
type Area = Double
type Pizza = (Size, Cost)

areaGivenParameter :: Size -> Area
areaGivenParameter size = pi * (size / 2) ^ 2

costPerCm :: (Size, Cost) -> Double
costPerCm (size, cost) = cost / areaGivenParameter size

costData :: M.Map Int Cost
costData = M.fromList [(1, 150), (2, 220)]

sizeData :: M.Map Int Size
sizeData = M.fromList [(1, 30), (2, 50)]

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2 then p1 else p2
  where
    costP1 = costPerCm p1
    costP2 = costPerCm p2

describePizza :: (Size, Cost) -> [Char]
describePizza (size, cost) = "Pizza with size " ++ show size ++ " is cheaper by " ++ show costSqCm ++ " per cm^2"
    where costSqCm = costPerCm (size, cost)

mainWithDo :: IO ()
mainWithDo = do
    putStrLn "First pizza size:"
    size1 <- getLine
    putStrLn "First pizza cost:"
    cost1 <- getLine
    putStrLn "Second pizza size:"
    size2 <- getLine
    putStrLn "Second pizza cost:"
    cost2 <- getLine
    let pizza1      = (read size1, read cost1)
    let pizza2      = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)

main :: IO ()
main =
    putStrLn "First pizza size:"
        >>  getLine
        >>= (\size1 ->
                putStrLn "First pizza cost:"
                    >>  getLine
                    >>= (\cost1 ->
                            putStrLn "Second pizza size:"
                                >>  getLine
                                >>= (\size2 ->
                                        putStrLn "Second pizza cost"
                                            >>  getLine
                                            >>= (\cost2 ->
                                                    (\pizza1 ->
                                                            (\pizza2 ->
                                                                    (\betterPizza -> putStrLn (describePizza betterPizza))
                                                                        (comparePizzas pizza1 pizza2)
                                                                )
                                                                (read size2, read cost2)
                                                        )
                                                        (read size1, read cost1)
                                                )
                                    )
                        )
            )

maybeMain :: Maybe String
maybeMain = do
    size1 <- M.lookup 1 sizeData
    cost1 <- M.lookup 1 costData
    size2 <- M.lookup 2 sizeData
    cost2 <- M.lookup 2 costData
    let pizza1      = (size1, cost1)
    let pizza2      = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

listMain :: [String]
listMain = do
    size1 <- (map snd . M.toList) sizeData
    cost1 <- (map snd . M.toList) costData
    size2 <- (map snd . M.toList) sizeData
    cost2 <- (map snd . M.toList) costData
    let pizza1      = (size1, cost1)
    let pizza2      = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)

monadMain :: Monad m => m Size -> m Cost -> m Size -> m Cost -> m [Char]
monadMain s1 c1 s2 c2 = do
    size1 <- s1
    cost1 <- c1
    size2 <- s2
    cost2 <- c2
    let pizza1      = (size1, cost1)
    let pizza2      = (size2, cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)
