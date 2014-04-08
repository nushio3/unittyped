{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Constants hiding (pi)
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived.Length
import UnitTyped.SI.Derived.Mass
import UnitTyped.SI.Derived.Time
import qualified UnitTyped.NoPrelude as D

-- The distance
fromPUtoWH :: Double :| Mile
fromPUtoWH = 137 *| mile

fuelEfficiency :: Value '[ '(Length, NTwo) ] '[  '(Gallon, NOne), '(Mile, POne) ] Double
fuelEfficiency = autoc $ 40 *| mile |/| gallon

gasolineDensity :: Value '[ '(Mass, POne), '(Length, NThree) ] '[  '(Gallon, NOne), '(Pound, POne) ] Double
gasolineDensity = autoc $ 7.29 *| pound |/| gallon

fromPUtoWHMetric :: Double :| Kilo Meter
fromPUtoWHMetric = autoc $ fromPUtoWH

fuelEfficiencyMetric :: Value '[ '(Length, NTwo) ] '[  '(Liter, NOne), '(Kilo Meter, POne) ] Double 
fuelEfficiencyMetric = autoc $ fuelEfficiency

gasolineDensityMetric :: Value '[ '(Mass, POne), '(Length, NThree) ] '[  '(Liter, NOne), '(Kilo Gram, POne) ] Double
gasolineDensityMetric = autoc $ gasolineDensity



main :: IO ()
main = do
  putStrLn $ "The distance between University of Pennsylvania and The White House is " ++ show fromPUtoWH
  putStrLn $ "The fuel economy of our car is " ++ show fuelEfficiency
  putStrLn $ "The density of gasoline is " ++ show gasolineDensity
  putStrLn $ "The mass of gasoline we need is " ++ show (fromPUtoWH |/| fuelEfficiency |*| gasolineDensity)

  putStrLn ""

  putStrLn $ "The distance between University of Pennsylvania and The White House is " ++ show fromPUtoWHMetric
  putStrLn $ "The fuel economy of our car is " ++ show fuelEfficiencyMetric
  putStrLn $ "The density of gasoline is " ++ show gasolineDensityMetric
  putStrLn $ "The mass of gasoline we need is " ++ show (fromPUtoWHMetric |/| fuelEfficiencyMetric |*| gasolineDensityMetric)