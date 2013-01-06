{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, TypeFamilies #-}
module Main where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived
import UnitTyped.SI.Derived.Length
import UnitTyped.SI.Derived.Mass
import UnitTyped.SI.Derived.Count
import UnitTyped.SI.Derived.Time
import UnitTyped.SI.Meta
import UnitTyped.SI.Constants
import UnitTyped.Bytes
import UnitTyped.NoPrelude
import UnitTyped.SI.Show

import qualified Prelude
import Control.Monad (foldM, unless)
import Data.Ratio
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Prelude (zip, show, (++), IO, Bool(..), Integer, Double, return, error, putStrLn, fromIntegral)
import System.Exit (exitFailure)

x1 :: Value LengthDimension (U Meter) Rational
x1 = 1 *| meter

t1 = x1 == 72 *| centi meter + 280 *| mili meter


vx0 :: V.Vector (Value LengthDimension (U Meter) Rational)
vx0 = V.generate 120 (\i -> fromIntegral i *| meter)

vux0d :: VU.Vector (Value LengthDimension (U Meter) Double)
vux0d = VU.generate 120 (\i -> fromIntegral i *| meter)


runTest :: Bool -> (Bool, Integer) -> IO Bool
runTest b (True, _) = return b
runTest b (False, i) = do { putStrLn ("Test " ++ show i ++ " failed.")
                                                  ; return False
                                                  }

main = do { b <- foldM runTest True (zip [t1] [1..])
                  ; unless b exitFailure
                  }