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
import qualified Data.Vector.Unboxed as VU
import Prelude (zip, show, (++), IO, Bool(..), Integer, return, error, putStrLn)
import System.Exit (exitFailure)


t1 = hertz == (1 /| second)

runTest :: Bool -> (Bool, Integer) -> IO Bool
runTest b (True, _) = return b
runTest b (False, i) = do { putStrLn ("Test " ++ show i ++ " failed.")
                                                  ; return False
                                                  }

main = do { b <- foldM runTest True (zip [t1] [1..])
                  ; unless b exitFailure
                  }