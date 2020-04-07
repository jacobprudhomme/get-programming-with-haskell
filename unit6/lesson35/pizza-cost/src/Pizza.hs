module Pizza
  ( comparePizzas
  , describePizza
  ) where

import qualified Data.Map as Map


areaFromDiameter :: Double -> Double
areaFromDiameter diam = pi * (diam / 2)^2

type Pizza = (Double,Double)

costPerSqrInch :: Pizza -> Double
costPerSqrInch (diam, price) = price / size
  where size = areaFromDiameter diam

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if costP1 < costP2
  then p1
  else p2
  where
    costP1 = costPerSqrInch p1
    costP2 = costPerSqrInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = mconcat
  [ "The "
  , show size
  , " pizza is cheaper at "
  , show costPerArea
  , " per square inch"
  ]
  where costPerArea = costPerSqrInch (size, cost)
