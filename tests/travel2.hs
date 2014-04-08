{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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



gasolineWeight :: 
  ( Fractional val
  , Convertible' '[ '(Length, POne) ] uniLen
  , Convertible' '[ '(Length, NTwo) ] uniEf
  , Convertible' '[ '(Length, PThree) ]  uniVol
  , Convertible' '[ '(Mass, POne), '(Length, NThree) ] uniDen
  , Convertible' '[ '(Mass, POne) ] uniMass

  , MapNeg uniEf negUniEf
  , MapMerge uniLen negUniEf uniVol
  , MapMerge uniVol uniDen uniMass
  ) =>
     Value '[ '(Length, POne) ] uniLen val
  -> Value '[ '(Length, NTwo) ] uniEf val
  -> Value '[ '(Mass, POne), '(Length, NThree) ] uniDen val 
  -> Value '[ '(Mass, POne) ] uniMass val 

gasolineWeight len0 ef0 den0 = len0 |/| ef0 |*| den0

main :: IO ()
main = do
  putStrLn $ "The distance between University of Pennsylvania and The White House is " ++ show fromPUtoWH
  putStrLn $ "The fuel economy of our car is " ++ show fuelEfficiency
  putStrLn $ "The density of gasoline is " ++ show gasolineDensity
  putStrLn $ "The mass of gasoline we need is " ++ show (gasolineWeight fromPUtoWH fuelEfficiency gasolineDensity)

  putStrLn ""

  putStrLn $ "The distance between University of Pennsylvania and The White House is " ++ show fromPUtoWHMetric
  putStrLn $ "The fuel economy of our car is " ++ show fuelEfficiencyMetric
  putStrLn $ "The density of gasoline is " ++ show gasolineDensityMetric
  putStrLn $ "The mass of gasoline we need is " ++ 
    show (gasolineWeight fromPUtoWHMetric fuelEfficiencyMetric gasolineDensityMetric)


{--- execution result ---

The distance between University of Pennsylvania and The White House is 137.0 mile
The fuel economy of our car is 40.0 mile⋅gallon⁻¹
The density of gasoline is 7.29 lb⋅gallon⁻¹
The mass of gasoline we need is 24.968249999999998 lb

The distance between University of Pennsylvania and The White House is 220.480128 km
The fuel economy of our car is 14.160247597385885 km⋅L⁻¹
The density of gasoline is 0.7273697567140114 kg⋅L⁻¹
The mass of gasoline we need is 11.3254076922525 kg

-}