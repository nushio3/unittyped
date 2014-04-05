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
fromGLtoED :: Float :| Mile
fromGLtoED = 46.5 *| mile

fuelEfficiency :: Value '[ '(Length, NTwo) ] '[  '(Gallon, NOne), '(Mile, POne) ] Float
fuelEfficiency = autoc $ 40 *| mile |/| gallon

gasolineDensity :: Value '[ '(Mass, POne), '(Length, NThree) ] '[  '(Gallon, NOne), '(Pound, POne) ] Float
gasolineDensity = autoc $ 7.29 *| pound |/| gallon

fromGLtoEDMetric :: Float :| Kilo Meter
fromGLtoEDMetric = autoc $ fromGLtoED

fuelEfficiencyMetric :: Value '[ '(Length, NTwo) ] '[  '(Liter, NOne), '(Kilo Meter, POne) ] Float 
fuelEfficiencyMetric = autoc $ fuelEfficiency

gasolineDensityMetric :: Value '[ '(Mass, POne), '(Length, NThree) ] '[  '(Liter, NOne), '(Kilo Gram, POne) ] Float
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
  putStrLn $ "distance: " ++ show fromGLtoED
  putStrLn $ "economy: " ++ show fuelEfficiency
  putStrLn $ "density: " ++ show gasolineDensity
  putStrLn $ "mass: " ++ show (gasolineWeight fromGLtoED fuelEfficiency gasolineDensity)

  putStrLn ""

  putStrLn $ "distance: " ++ show fromGLtoEDMetric
  putStrLn $ "economy: " ++ show fuelEfficiencyMetric
  putStrLn $ "density: " ++ show gasolineDensityMetric
  putStrLn $ "mass: " ++ 
    show (gasolineWeight fromGLtoEDMetric fuelEfficiencyMetric gasolineDensityMetric)


{--- execution result ---

distance: 46.5 mile
economy: 40.0 mile⋅gallon⁻¹
density: 7.29 lb⋅gallon⁻¹
mass: 8.474626 lb

distance: 74.834496 km
economy: 14.160249 km⋅L⁻¹
density: 0.7273698 kg⋅L⁻¹
mass: 3.8440251 kg


-}