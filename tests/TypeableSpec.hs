{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses, FunctionalDependencies, ExistentialQuantification, TypeFamilies #-}

module TypeableSpec where

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
import UnitTyped.SI.Show

import           Data.Dynamic
import           Test.Hspec
import           Test.Hspec.Expectations
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck

infix 4 `isoquant`


isoquant a b = a == b `shouldBe` True

shouldTypeCheck a = a `shouldSatisfy` const True

spec :: Spec
spec = do
  let ans = 42 :: Double
  describe "SI units TypeRep" $ do
    it "distinguishes units." $ do
      ((typeOf $ ans *| meter) == (typeOf $ ans *| second)) `shouldBe` False
    prop "is not affected by internal values" $ \x y ->
      let _ = [x,y,ans] in ((typeOf $ x *| meter) == (typeOf $ y *| meter))
    prop "can extract the original values from the dynamic" $ \x ->
      let _ = [x,ans]
          mx = x *| meter
          dx = toDyn mx
          mx'= fromDynamic dx
      in mx' == Just mx
