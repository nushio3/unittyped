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

t15 = True -- 3.6 kilo meter / hour == 1 meter / second
t16 = True -- 3.6 mega joule == 1 kilo watt * hour
t17 = 1 ~> cubic (deci meter) == 1 liter
t18 = 1 ~> square meter == 10000 ~> square (centi meter)

t19 = (1 meter / second) * (1 second) == 1 meter

-- These should just typecheck
t20 = mile + inch + yard + foot + ångström + nautical_mile + meter == mile + inch + yard + foot + ångström + nautical_mile + meter
t21 = UnitTyped.SI.Derived.Mass.pound + kilo gram == UnitTyped.SI.Derived.Mass.pound + kilo gram

t22 = minute + hour + day + year + julian_year + month + second == minute + hour + day + year + julian_year + month + second
t23 = second * hertz == count

t24 = percent + permil + ppm + ppb + ppt == percent + permil + ppm + ppb + ppt

t25 = hbar == h / (2 count * pi)

t26 = pown3 meter * pow3 meter == pown1 meter * pow1 meter
t27 = pown3 meter * pow3 meter == pown2 meter * pow2 meter
t28 = pow0 meter == count

runTest :: Bool -> (Bool, Integer) -> IO Bool
runTest b (True, _) = return b
runTest b (False, i) = do { putStrLn ("Test " ++ show i ++ " failed.")
		   				  ; return False
						  }

main = do { b <- foldM runTest True (zip [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28] [1..])
		  ; unless b exitFailure
		  }