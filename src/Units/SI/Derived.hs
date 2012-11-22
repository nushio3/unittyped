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

data Mile = Mile

type Miles = (Fractional f) => Value f LengthUnit Mile

instance Show Mile where
	show _ = "mile"

instance Convertable LengthUnit Mile where
	factor _ = 1609
	constructor = Mile
--

data Inch = Inch

type Inches = (Fractional f) => Value f LengthUnit Inch

instance Show Inch where
	show _ = "in"

instance Convertable LengthUnit Inch where
	factor _ = 0.0254
	constructor = Inch

--

data Yard = Yard

type Yards = (Fractional f) => Value f LengthUnit Yard

instance Show Yard where
	show _ = "yd"

instance Convertable LengthUnit Yard where
	factor _ = 0.9144
	constructor = Yard

--

data Foot = Foot

type Feet = (Fractional f) => Value f LengthUnit Foot

instance Show Foot where
	show _ = "ft"

instance Convertable LengthUnit Foot where
	factor _ = 0.3048
	constructor = Foot

--

type VolumeUnit = UnitCons Length (Pos (Suc (Suc One))) UnitNil

data Liter = Liter

type Liters = (Fractional f) => Value f VolumeUnit Liter

instance Show Liter where
	show _ = "L"

instance Convertable VolumeUnit Liter where
	factor _ = 0.001
	constructor = Liter

data Gallon = Gallon

type Gallons = (Fractional f) => Value f VolumeUnit Gallon

instance Show Gallon where
	show _ = "gallon"

instance Convertable VolumeUnit Gallon where
	factor _ = 0.00454609
	constructor = Gallon

----
-- Time
----

data Hour = Hour
type Hours = (Fractional f) => Value f TimeUnit Hour

instance Show Hour where
	show _ = "h"

instance Convertable TimeUnit Hour where
	factor _ = 3600
	constructor = Hour

--

data Minute = Minute
type Minutes = (Fractional f) => Value f TimeUnit Minute

instance Show Minute where
	show _ = "min."

instance Convertable TimeUnit Minute where
	factor _ = 60
	constructor = Minute

--

data Day = Day
type Days = (Fractional f) => Value f TimeUnit Day

instance Show Day where
	show _ = "day"

instance Convertable TimeUnit Day where
	factor _ = 86400
	constructor = Day

--

data Herz = Herz
type Herzs = (Fractional f) => Value f (UnitCons Time (Neg One) UnitNil) Herz

instance Show Herz where
	show _ = "Hz"

instance Convertable (UnitCons Time (Neg One) UnitNil) Herz where
	factor _ = 1
	constructor = Herz


----
-- Mass
----

type Kilograms = (Fractional f) => Value f MassUnit (Kilo Gram)

data PlanckMass = PlanckMass
type PlanckMasses = (Fractional f) => Value f MassUnit PlanckMass

instance Show PlanckMass where
	show _ = "m_P"

instance Convertable MassUnit PlanckMass where
	factor _ = 2.176513e-8
	constructor = PlanckMass

----
-- Misc
----


type Speed = UnitCons Time (Neg One) (UnitCons Length (Pos One) UnitNil)
type Acceleration = UnitCons Time (Neg (Suc One)) (UnitCons Length (Pos One) UnitNil)

type Force = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos One) UnitNil))
data Newton = Newton
type Newtons = (Fractional f) => Value f Force Newton

instance Show Newton where
	show _ = "N"

instance Convertable Force Newton where
	factor _ = 1
	constructor = Newton

type Energy = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) UnitNil))
data Joule = Joule
type Joules = (Fractional f) => Value f Energy Joule
type Kwh = (Fractional f) => Value f Energy (Mul (Kilo Watt) Hour)

instance Show Joule where
	show _ = "J"

instance Convertable Energy Joule where
	factor _ = 1
	constructor = Joule

data Ev = Ev
type Evs = (Fractional f) => Value f Energy Ev

instance Show Ev where
	show _ = "eV"

instance Convertable Energy Ev where
	factor _ = 1.60217656535e-19
	constructor = Ev

type Power = UnitCons Time (Neg (Suc (Suc One))) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))
data Watt = Watt
type Watts = (Fractional f) => Value f Power Watt

instance Show Watt where
	show _ = "W"

instance Convertable Power Watt where
	factor _ = 1
	constructor = Watt

type Pressure = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Neg One) UnitNil))
data Pascal = Pascal
type Pascals = (Fractional f) => Value f Pressure Pascal

instance Show Pascal where
	show _ = "Pa"

instance Convertable Pressure Pascal where
	factor _ = 1
	constructor = Pascal

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