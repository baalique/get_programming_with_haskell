module Calculations where

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
