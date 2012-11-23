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
import UnitTyped.Bytes
import UnitTyped.Currency
import UnitTyped.NoPrelude

import qualified Prelude
import Control.Monad (foldM, unless)
import Prelude (zip, show, (++), IO, Bool(..), Integer, return, error, putStrLn)
import System.Exit (exitFailure)

t1 = hertz == (count / second)
t2 = rad == (meter / meter)
t3 = newton == kilo gram * meter / square second
t4 = pascal == newton / square meter
t5 = joule == newton * meter
t6 = watt == joule / second
t7 = coulomb == second * ampere
t8 = volt == watt / ampere
t9 = farad == coulomb / volt
t10 = ohm == volt / ampere
t11 = siemens == count / ohm
t12 = weber == joule / ampere
t13 = tesla == volt * second / square meter
t14 = henry == volt * second / ampere

t15 = 3.6 kilo meter / hour == 1 meter / second
t16 = 3.6 mega joule == 1 kilo watt * hour

runTest :: Bool -> (Bool, Integer) -> IO Bool
runTest b (True, _) = return b
runTest b (False, i) = do { putStrLn ("Test " ++ show i ++ " failed.")
		   				  ; return False
						  }

main = do { b <- foldM runTest True (zip [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16] [1..])
		  ; unless b exitFailure
		  }