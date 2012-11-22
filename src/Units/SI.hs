{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.SI where

import Units.Units

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..))

--

data Count = Count
type CountUnit = UnitNil

instance Show Count where
	show _ = "#"

instance Convertable CountUnit Count where
	factor _ = 1
	constructor = Count

----
-- Length
----

data Length = Length
type LengthUnit = UnitCons Length (Pos One) UnitNil

--

data Meter = Meter

type Meters = (Fractional f) => Value f LengthUnit Meter

instance Show Meter where
	show _ = "m"

instance Convertable LengthUnit Meter where
	factor _ = 1
	constructor = Meter

----
-- Time
----

data Time = Time
type TimeUnit = UnitCons Time (Pos One) UnitNil

--

data Second = Second

type Seconds = (Fractional f) => Value f TimeUnit Second

instance Show Second where
	show _ = "s"

instance Convertable TimeUnit Second where
	factor _ = 1
	constructor = Second

----
-- Mass
----

data Mass = Mass
type MassUnit = UnitCons Mass (Pos One) UnitNil

data Gram = Gram
type Grams = (Fractional f) => Value f MassUnit Gram

instance Show Gram where
	show _ = "g"

instance Convertable MassUnit Gram where
	factor _ = 0.001
	constructor = Gram

----

count :: (Fractional f) => f -> Value f CountUnit Count
count = mkVal

--

meter :: (Fractional f) => f -> Value f LengthUnit Meter
meter = mkVal

----

second :: (Fractional f) => f -> Value f TimeUnit Second
second = mkVal

--

gram :: (Fractional f) => f -> Value f MassUnit Gram
gram = mkVal
