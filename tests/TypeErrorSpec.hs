{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, TypeFamilies #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module TypeErrorSpec where

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
import           Prelude (zip, show, const, (++), IO, Bool(..), Integer, return, error, putStrLn, ($), Rational, print)
import           Test.Hspec
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           System.Exit (exitFailure)

infix 4 `isoquant`


isoquant a b = a == b `shouldBe` True

shouldTypeCheck a = a `shouldSatisfy` const True

spec :: Spec
spec = do
  describe "SI units typecheck Test" $ do
    it "detects addition between wrong units." $ do
      print (meter+second) `shouldThrow` anyException
