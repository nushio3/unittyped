{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module UnboxSpec where

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


import           Control.Monad (foldM, unless)
import           Data.Ratio
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Eval as Repa
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Prelude
import           Prelude (zip, show, (++), IO, Bool(..), Integer, Double, return, error, putStrLn, fromIntegral, ($))
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           System.Exit (exitFailure)

spec :: Spec
spec = do
  describe "Giving dimensional units to unboxed types." $ do
    return ()


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

t2 = (Repa.foldAllS (|+|) (0*| meter) rux2d) Prelude.== 4950 *| meter

deriving instance Repa.Elt f => Repa.Elt (Value a b f)

runTest :: Bool -> (Bool, Integer) -> IO Bool
runTest b (True, _) = return b
runTest b (False, i) = do { putStrLn ("Test " ++ show i ++ " failed.")
                                                  ; return False
                                                  }

main = do { b <- foldM runTest True (zip [t1, t2] [1..])
                  ; unless b exitFailure
                  }