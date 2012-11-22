{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.SI.Derived where

import Units.Units
import Units.SI
import Units.SI.Meta

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..))


----
-- Length
----

type Kilometers = (Fractional f) => Value f LengthUnit (Kilo Meter)
type Milimeters = (Fractional f) => Value f LengthUnit (Mili Meter)

--

data Mile
type Miles = (Fractional f) => Value f LengthUnit Mile

instance Convertable LengthUnit Mile where
	factor _ = 1609
	showunit _ = "mile"
--

data Inch
type Inches = (Fractional f) => Value f LengthUnit Inch

instance Convertable LengthUnit Inch where
	factor _ = 0.0254
	showunit _ = "in"

--

data Yard
type Yards = (Fractional f) => Value f LengthUnit Yard

instance Convertable LengthUnit Yard where
	factor _ = 0.9144
	showunit _ = "yd"

--

data Foot
type Feet = (Fractional f) => Value f LengthUnit Foot

instance Convertable LengthUnit Foot where
	factor _ = 0.3048
	showunit _ = "ft"

--

type VolumeUnit = UnitCons Length (Pos (Suc (Suc One))) UnitNil

data Liter
type Liters = (Fractional f) => Value f VolumeUnit Liter

instance Convertable VolumeUnit Liter where
	factor _ = 0.001
	showunit _ = "L"

data Gallon
type Gallons = (Fractional f) => Value f VolumeUnit Gallon

instance Convertable VolumeUnit Gallon where
	factor _ = 0.00454609
	showunit _ = "gallon"

----
-- Time
----

data Hour
type Hours = (Fractional f) => Value f TimeUnit Hour

instance Convertable TimeUnit Hour where
	factor _ = 3600
	showunit _ = "h"

--

data Minute
type Minutes = (Fractional f) => Value f TimeUnit Minute

instance Convertable TimeUnit Minute where
	factor _ = 60
	showunit _ = "min."

--

data Day
type Days = (Fractional f) => Value f TimeUnit Day

instance Convertable TimeUnit Day where
	factor _ = 86400
	showunit _ = "day"

--

data Herz
type Herzs = (Fractional f) => Value f (UnitCons Time (Neg One) UnitNil) Herz

instance Convertable (UnitCons Time (Neg One) UnitNil) Herz where
	factor _ = 1
	showunit _ = "Hz"


----
-- Mass
----

type Kilograms = (Fractional f) => Value f MassUnit (Kilo Gram)

data PlanckMass
type PlanckMasses = (Fractional f) => Value f MassUnit PlanckMass

instance Convertable MassUnit PlanckMass where
	factor _ = 2.176513e-8
	showunit _ = "m_P"

----
-- Misc
----


type Speed = UnitCons Time (Neg One) (UnitCons Length (Pos One) UnitNil)
type Acceleration = UnitCons Time (Neg (Suc One)) (UnitCons Length (Pos One) UnitNil)

type Force = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos One) UnitNil))
data Newton
type Newtons = (Fractional f) => Value f Force Newton

instance Convertable Force Newton where
	factor _ = 1
	showunit _ = "N"

type Energy = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) UnitNil))
data Joule
type Joules = (Fractional f) => Value f Energy Joule
type Kwh = (Fractional f) => Value f Energy (Mul (Kilo Watt) Hour)

instance Convertable Energy Joule where
	factor _ = 1
	showunit _ = "J"

data Ev
type Evs = (Fractional f) => Value f Energy Ev

instance Convertable Energy Ev where
	factor _ = 1.60217656535e-19
	showunit _ = "eV"

type Power = UnitCons Time (Neg (Suc (Suc One))) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))
data Watt
type Watts = (Fractional f) => Value f Power Watt

instance Convertable Power Watt where
	factor _ = 1
	showunit _ = "W"

type Pressure = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Neg One) UnitNil))
data Pascal
type Pascals = (Fractional f) => Value f Pressure Pascal

instance Convertable Pressure Pascal where
	factor _ = 1
	showunit _ = "Pa"

--

kilometer = kilo meter
milimeter = mili meter

mile :: (Fractional f) => f -> Value f LengthUnit Mile
mile = mkVal

inch :: (Fractional f) => f -> Value f LengthUnit Inch
inch = mkVal

yard :: (Fractional f) => f -> Value f LengthUnit Yard
yard = mkVal

foot :: (Fractional f) => f -> Value f LengthUnit Foot
foot = mkVal

--

liter :: (Fractional f) => f -> Value f VolumeUnit Liter
liter = mkVal

gallon :: (Fractional f) => f -> Value f VolumeUnit Gallon
gallon = mkVal

--

minute :: (Fractional f) => f -> Value f TimeUnit Minute
minute = mkVal

hour :: (Fractional f) => f -> Value f TimeUnit Hour
hour = mkVal

day :: (Fractional f) => f -> Value f TimeUnit Day
day = mkVal

--

kilogram :: (Fractional f) => f -> Value f MassUnit (Kilo Gram)
kilogram = mkVal

m_P :: (Fractional f) => Value f MassUnit PlanckMass
m_P = mkVal 1

--

newton :: (Fractional f) => f -> Value f Force Newton
newton = mkVal

joule :: (Fractional f) => f -> Value f Energy Joule
joule = mkVal

eV :: (Fractional f) => f -> Value f Energy Ev
eV = mkVal

kwh :: (Fractional f) => f -> Value f Energy (Mul (Kilo Watt) Hour)
kwh = mkVal

watt :: (Fractional f) => f -> Value f Power Watt
watt = mkVal

pascal :: (Fractional f) => f -> Value f Pressure Pascal
pascal = mkVal