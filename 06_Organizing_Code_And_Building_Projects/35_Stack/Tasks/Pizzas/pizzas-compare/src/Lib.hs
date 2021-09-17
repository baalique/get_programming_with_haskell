module Lib where

import           Calculations                   ( Cost
                                                , Size
                                                , comparePizzas
                                                , costPerCm
                                                )

describePizza :: (Size, Cost) -> [Char]
describePizza (size, cost) = "Pizza with size " ++ show size ++ " is cheaper by " ++ show costSqCm ++ " per cm^2"
    where costSqCm = costPerCm (size, cost)
