{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Constants hiding (pi)
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived.Time
import qualified UnitTyped.NoPrelude as D

-- The mass of the sun
solarMass :: Double :| Kilo Gram
solarMass = mkVal 1.98892e30

-- The semi-major axis of the Earth orbit
earthOrbitRadius :: Double :| Meter
earthOrbitRadius = mkVal 1.49597871e11

op :: Double :| Second
op = autoc $ (2 * pi) /| y
   where
     y :: Value '[ '(Time, NOne) ] '[ '(Second, NOne) ] Double
     y = D.sqrt x
     x :: Value '[ '(Time, NTwo) ] '[ '(Second, NTwo) ] Double
     x = g |*| solarMass |/| cubic earthOrbitRadius

main :: IO ()
main = do
  putStrLn $ "The mass of the sun is " ++ show solarMass
  putStrLn $ "The radius of the Earth orbit is " ++ show earthOrbitRadius
  putStrLn $ "The orbital period of the Earth is " ++ show op
  putStrLn $ "  which is " ++ show (autoc op :: Double :| Day)
