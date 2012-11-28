{-# LANGUAGE DataKinds #-}
-- |A module with dimensions and units derived from combining SI units.
module UnitTyped.SI.Derived where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived.Time
import UnitTyped.SI.Meta

import Data.Ratio

-- |Speed. @Length^1 Time^-1@.
type Speed = '[ '(Time, NOne), '(Length, POne) ]
-- |Acceleration. @Length^1 Time^-2@.
type Acceleration = '[ '(Time, NTwo), '(Length, POne) ]

-- |Derived unit of speed (kn).
data Knot

instance Convertible Speed Knot where
	factor _ = 1852 / 3600
	showunit _ = "kn"

--

-- |Force. @Length^1 Time^-2 Mass^1@.
type Force = '[ '(Time, NTwo), '(Mass, POne), '(Length, POne) ]
-- |Unit of force (N).
data Newton

instance Convertible Force Newton where
	factor _ = 1
	showunit _ = "N"

--

-- |Energy. @Length^2 Time^-2 Mass^1@.
type Energy = '[ '(Time, NTwo), '(Mass, POne), '(Length, PTwo) ]

-- |Unit of energy (J).
data Joule

instance Convertible Energy Joule where
	factor _ = 1
	showunit _ = "J"

-- |Unit of energy (eV).
data Ev

instance Convertible Energy Ev where
	factor _ = 1.60217656535e-19
	showunit _ = "eV"

--

-- |Energy. @Length^2 Time^-3 Mass^1@.
type Power = '[ '(Time, NThree), '(Length, PTwo), '(Mass, POne) ]

-- |Unit of power (W).
data Watt

instance Convertible Power Watt where
	factor _ = 1
	showunit _ = "W"

--

-- |Energy. @Length^-1 Time^-2 Mass^1@.
type Pressure = '[ '(Time, NTwo), '(Mass, POne), '(Length, NOne) ]

-- |Unit of pressure (Pa).
data Pascal

instance Convertible Pressure Pascal where
	factor _ = 1
	showunit _ = "Pa"

-- |Unit of pressure (bar).
data Bar

instance Convertible Pressure Bar where
	factor _ = 1e5
	showunit _ = "bar"

-- |Unit of pressure (mmHg).
data MmHg

instance Convertible Pressure MmHg where
	factor _ = 133.322
	showunit _ = "mmHg"

--

-- |Electric charge. @Time^1 Current^1@.
type Charge = '[ '(Time, POne), '(Current, POne) ]

-- |Unit of chage (C).
data Coulomb

instance Convertible Charge Coulomb where
	factor _ = 1
	showunit _ = "C"

--

-- |Electric potential. @Time^-3 Current^-1 Mass^1 Length^2@.
type Potential = '[ '(Current, NOne), '(Mass, POne), '(Length, PTwo), '(Time, NThree) ]

-- |Unit of potential (V).
data Volt

instance Convertible Potential Volt where
	factor _ = 1
	showunit _ = "V"

--

-- |Electric capacitance. @Current^2 Mass^-1 Length^2 Time^4@.
type Capacitance = '[ '(Current, PTwo), '(Mass, NOne), '(Length, NTwo), '(Time, PFour) ]

-- |Unit of capacitance (F).
data Farad

instance Convertible Capacitance Farad where
	factor _ = 1
	showunit _ = "F"

--

-- |Electric resistance. @Current^-2 Time^-3 Length^2 Mass^1@.
type Resistance = '[ '(Current, NTwo), '(Time, NThree), '(Length, PTwo), '(Mass, POne) ]

-- |Unit of resistance (Ω).
data Ohm

instance Convertible Resistance Ohm where
	factor _ = 1
	showunit _ = "Ω"

--

-- |Electric conductance. @Current^2 Mass^-1 Length^-2 Time^3@.
type Conductance = '[ '(Current, PTwo), '(Mass, NOne), '(Length, NTwo), '(Time, PThree) ]

-- |Unit of conductance (S).
data Siemens

instance Convertible Conductance Siemens where
	factor _ = 1
	showunit _ = "S"

--

-- |Magnetic flux. @Current^-1 Length^2 Mass^1 Time^-2@.
type Flux = '[ '(Current, NOne), '(Length, PTwo), '(Mass, POne), '(Time, NTwo) ]

-- |Unit of magnetic flux (Wb).
data Weber

instance Convertible Flux Weber where
	factor _ = 1
	showunit _ = "Wb"

--

-- |Magnetic field strength. @Time^-2 Mass^1 Current^-1@.
type FluxDensity = '[ '(Time, NTwo), '(Mass, POne), '(Current, NOne) ]

-- |Unit of magnetic field strength (T).
data Tesla

instance Convertible FluxDensity Tesla where
	factor _ = 1
	showunit _ = "T"

--

-- |Inductance. @Current^-2 Time^-2 Mass^1 Length^2@.
type Inductance = '[ '(Current, NTwo), '(Time, NTwo), '(Mass, POne), '(Length, PTwo) ]

-- |Unit of Inductance (H).
data Henry

instance Convertible Inductance Henry where
	factor _ = 1
	showunit _ = "H"

--

-- |One knot.
knot :: (Fractional f) => Value f Speed (Unit Knot)
knot = one

-- |One newton.
newton :: (Fractional f) => Value f '[ '(Time, NTwo), '(Mass, POne), '(Length, POne)] (Unit Newton)
newton = one

-- |One joule.
joule :: (Fractional f) => Value f Energy (Unit Joule)
joule = one

-- |One eV.
eV :: (Fractional f) => Value f Energy (Unit Ev)
eV = one

-- |One kwh.
kwh :: (Fractional f) => Value f Energy '[ '(Kilo Watt, POne), '(Hour, POne) ]
kwh = one

-- |One watt.
watt :: (Fractional f) => Value f Power (Unit Watt)
watt = one

-- |One pascal.
pascal :: (Fractional f) => Value f Pressure (Unit Pascal)
pascal = one

-- |One bar.
bar :: (Fractional f) => Value f Pressure (Unit Bar)
bar = one

-- |One mmHg.
mmHg :: (Fractional f) => Value f Pressure (Unit MmHg)
mmHg = one

-- |One coulomb.
coulomb :: (Fractional f) => Value f Charge (Unit Coulomb)
coulomb = one

-- |One volt.
volt :: (Fractional f) => Value f Potential (Unit Volt)
volt = one

-- |One farad.
farad :: (Fractional f) => Value f Capacitance (Unit Farad)
farad = one

-- |One ohm.
ohm :: (Fractional f) => Value f Resistance (Unit Ohm)
ohm = one

-- |One siemens.
siemens :: (Fractional f) => Value f Conductance (Unit Siemens)
siemens = one

-- |One weber.
weber :: (Fractional f) => Value f Flux (Unit Weber)
weber = one

-- |One tesla.
tesla :: (Fractional f) => Value f FluxDensity (Unit Tesla)
tesla = one

-- |One henry.
henry :: (Fractional f) => Value f Inductance (Unit Henry)
henry = one
