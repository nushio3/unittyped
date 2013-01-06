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
import qualified Data.Array.Repa as Repa
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Prelude (zip, show, (++), IO, Bool(..), Integer, Double, return, error, putStrLn, fromIntegral)
import System.Exit (exitFailure)

x1 :: Value LengthDimension (U Meter) Rational
x1 = 1 *| meter

t1 = x1 == 72 *| centi meter + 280 *| mili meter


type DoubleMeter = Value LengthDimension (U Meter) Double

vx2 :: V.Vector (Value LengthDimension (U Meter) Rational)
vx2 = V.generate 100 (\i -> fromIntegral i *| meter)

vux2d :: VU.Vector DoubleMeter
vux2d = VU.generate 100 (\i -> fromIntegral i *| meter)

rux2d :: Repa.Array Repa.U Repa.DIM1 DoubleMeter
rux2d = Repa.fromUnboxed (Repa.ix1 100) vux2d

-- t2 = (Repa.sumAllS rux2d) == 4950
t2 = True

runTest :: Bool -> (Bool, Integer) -> IO Bool
runTest b (True, _) = return b
runTest b (False, i) = do { putStrLn ("Test " ++ show i ++ " failed.")
                                                  ; return False
                                                  }

main = do { b <- foldM runTest True (zip [t1, t2] [1..])
                  ; unless b exitFailure
                  }