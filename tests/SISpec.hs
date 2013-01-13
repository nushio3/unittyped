{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, TypeFamilies #-}

module SISpec where

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
import qualified Prelude
import           Prelude (zip, show, const, (++), IO, Bool(..), Integer, return, error, putStrLn, ($), Rational)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           System.Exit (exitFailure)

infix 4 `isoquant`


isoquant a b = a == b `shouldBe` True

shouldTypeCheck a = a `shouldSatisfy` const True

spec :: Spec
spec = do
  describe "SI units equivalence Test" $ do
    it "converts equivalent units." $ do
      hertz `isoquant` 1 /| second
      rad `isoquant` meter / meter
      newton `isoquant` kilo gram * meter / square second
      pascal `isoquant` newton / square meter
      joule `isoquant` newton * meter
      watt `isoquant` joule / second
      coulomb `isoquant` second * ampere
      volt `isoquant` watt / ampere
      farad `isoquant` coulomb / volt
      ohm `isoquant` volt / ampere
      siemens `isoquant` count / ohm
      weber `isoquant` joule / ampere
      tesla `isoquant` volt * second / square meter
      henry `isoquant` volt * second / ampere


      3.6 *| kilo meter / hour `isoquant` 1 *| meter / second
      3.6 *| mega joule `isoquant` 1 *| kilo watt * hour
      1 *| cubic (deci meter) `isoquant` 1 *| liter
      1 *| square meter `isoquant` 10000 *| square (centi meter)
      (1 *| meter / second) * (1 *| second) `isoquant` 1 *| meter

    it "handles addition between different units of the same dimension." $ do
      shouldTypeCheck $ mile + inch + yard + foot + ångström + nautical_mile + meter 
      shouldTypeCheck $ UnitTyped.SI.Derived.Mass.pound + kilo gram
      shouldTypeCheck $ minute + hour + day + year + julian_year + month + second 
      shouldTypeCheck $ percent + permil + ppm + ppb + ppt 


    it "has well-defined constants." $ do
      second * hertz `isoquant` count
      hbar `isoquant` (h / (2 *| pi))
      let appleMass = 2.49 *| kilo gram 
      2.49e9 *| gram `isoquant` mega appleMass


