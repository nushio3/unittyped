{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.SI.Derived where

import Units.Units
import Units.SI
import Units.SI.Meta

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..))


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
	factor _ = 2.176513e-5
	constructor = PlanckMass

----
-- Misc
----


type Speed = UnitCons Time (Neg One) (UnitCons Length (Pos One) UnitNil)
type Acceleration = UnitCons Time (Neg (Suc One)) (UnitCons Length (Pos One) UnitNil)
type Kmph = (Fractional f) => Value f Speed (Div (Kilo Meter) Hour)
type Mpss = (Fractional f) => Value f Acceleration (Div Meter (Mul Second Second))

type Force = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos One) UnitNil))
data Newton = Newton
type Newtons = (Fractional f) => Value f Force Newton

instance Show Newton where
	show _ = "N"

instance Convertable Force Newton where
	factor _ = 1000
	constructor = Newton

type Energy = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) UnitNil))
data Joule = Joule
type Joules = (Fractional f) => Value f Energy Joule
type Kwh = (Fractional f) => Value f Energy (Mul (Kilo Watt) Hour)

instance Show Joule where
	show _ = "J"

instance Convertable Energy Joule where
	factor _ = 1000
	constructor = Joule

type Power = UnitCons Time (Neg (Suc (Suc One))) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))
data Watt = Watt
type Watts = (Fractional f) => Value f Power Watt

instance Show Watt where
	show _ = "W"

instance Convertable Power Watt where
	factor _ = 1000
	constructor = Watt

--

kilometer = kilo meter
milimeter = mili meter

mile :: (Fractional f) => f -> Value f LengthUnit Mile
mile = mkVal

inch :: (Fractional f) => f -> Value f LengthUnit Inch
inch = mkVal

yard :: (Fractional f) => f -> Value f LengthUnit Yard
yard = mkVal

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

kwh :: (Fractional f) => f -> Value f Energy (Mul (Kilo Watt) Hour)
kwh = mkVal

watt :: (Fractional f) => f -> Value f Power Watt
watt = mkVal