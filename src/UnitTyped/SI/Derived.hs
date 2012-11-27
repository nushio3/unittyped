{-# LANGUAGE DataKinds #-}
-- |A module with dimensions and units derived from combining SI units.
module UnitTyped.SI.Derived where

import UnitTyped
import UnitTyped.SI
-- import UnitTyped.SI.Derived.Time
-- import UnitTyped.SI.Meta

import Data.Ratio

-- |Speed. @Length^1 Time^-1@.
type Speed = UnitCons Time NOne (UnitCons Length POne UnitNil)
-- |Acceleration. @Length^1 Time^-2@.
type Acceleration = UnitCons Time NTwo (UnitCons Length POne UnitNil)

-- |Derived unit of speed (kn).
data Knot

instance Convertible Speed Knot where
	factor _ = 1852 / 3600
	showunit _ = "kn"

--

-- |Force. @Length^1 Time^-1 Mass^1@.
type Force = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length POne UnitNil))
-- |Unit of force (N).
data Newton

instance Convertible Force Newton where
	factor _ = 1
	showunit _ = "N"

--

-- |Energy. @Length^2 Time^-2 Mass^1@.
type Energy = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length PTwo UnitNil))

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
type Power = UnitCons Time NThree (UnitCons Length PTwo (UnitCons Mass POne UnitNil))

-- |Unit of power (W).
data Watt

instance Convertible Power Watt where
	factor _ = 1
	showunit _ = "W"

--

-- |Energy. @Length^-1 Time^-2 Mass^1@.
type Pressure = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length NOne UnitNil))

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
type Charge = (UnitCons Time POne (UnitCons Current POne UnitNil))

-- |Unit of chage (C).
data Coulomb

instance Convertible Charge Coulomb where
	factor _ = 1
	showunit _ = "C"

--

-- |Electric potential. @Time^-3 Current^-1 Mass^1 Length^2@.
type Potential = (UnitCons Current NOne (UnitCons Mass POne (UnitCons Length PTwo (UnitCons Time NThree UnitNil))))

-- |Unit of potential (V).
data Volt

instance Convertible Potential Volt where
	factor _ = 1
	showunit _ = "V"

--

-- |Electric capacitance. @Current^2 Mass^-1 Length^2 Time^4@.
type Capacitance = (UnitCons Current PTwo (UnitCons Mass NOne (UnitCons Length NTwo (UnitCons Time PFour UnitNil))))

-- |Unit of capacitance (F).
data Farad

instance Convertible Capacitance Farad where
	factor _ = 1
	showunit _ = "F"

--

-- |Electric resistance. @Current^-2 Time^-3 Length^2 Mass^1@.
type Resistance = (UnitCons Current NTwo (UnitCons Time NThree (UnitCons Length PTwo (UnitCons Mass POne UnitNil))))

-- |Unit of resistance (Ω).
data Ohm

instance Convertible Resistance Ohm where
	factor _ = 1
	showunit _ = "Ω"

--

-- |Electric conductance. @Current^2 Mass^-1 Length^-2 Time^3@.
type Conductance = (UnitCons Current PTwo (UnitCons Mass NOne (UnitCons Length NTwo (UnitCons Time PThree UnitNil))))

-- |Unit of conductance (S).
data Siemens

instance Convertible Conductance Siemens where
	factor _ = 1
	showunit _ = "S"

--

-- |Magnetic flux. @Current^-1 Length^2 Mass^1 Time^-2@.
type Flux = (UnitCons Current NOne (UnitCons Length PTwo (UnitCons Mass POne (UnitCons Time NTwo UnitNil))))

-- |Unit of magnetic flux (Wb).
data Weber

instance Convertible Flux Weber where
	factor _ = 1
	showunit _ = "Wb"

--

-- |Magnetic field strength. @Time^-2 Mass^1 Current^-1@.
type FluxDensity = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Current NOne UnitNil))

-- |Unit of magnetic field strength (T).
data Tesla

instance Convertible FluxDensity Tesla where
	factor _ = 1
	showunit _ = "T"

--

-- |Inductance. @Current^-2 Time^-2 Mass^1 Length^2@.
type Inductance = (UnitCons Current NTwo (UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length PTwo UnitNil))))

-- |Unit of Inductance (H).
data Henry

instance Convertible Inductance Henry where
	factor _ = 1
	showunit _ = "H"

--

-- |One knot.
knot :: (Fractional f) => Value f Speed (UnitCons Knot POne UnitNil)
knot = one

-- |One newton.
newton :: (Fractional f) => Value f Force (UnitCons Newton POne UnitNil)
newton = one

-- |One joule.
joule :: (Fractional f) => Value f Energy (UnitCons Joule POne UnitNil)
joule = one

-- |One eV.
eV :: (Fractional f) => Value f Energy (UnitCons Ev POne UnitNil)
eV = one

-- |One kwh.
--kwh :: (Fractional f) => Value f Energy (UnitCons (Mul (Kilo Watt) Hour) POne UnitNil)
--kwh = one

-- |One watt.
watt :: (Fractional f) => Value f Power (UnitCons Watt POne UnitNil)
watt = one

-- |One pascal.
pascal :: (Fractional f) => Value f Pressure (UnitCons Pascal POne UnitNil)
pascal = one

-- |One bar.
bar :: (Fractional f) => Value f Pressure (UnitCons Bar POne UnitNil)
bar = one

-- |One mmHg.
mmHg :: (Fractional f) => Value f Pressure (UnitCons MmHg POne UnitNil)
mmHg = one

-- |One coulomb.
coulomb :: (Fractional f) => Value f Charge (UnitCons Coulomb POne UnitNil)
coulomb = one

-- |One volt.
volt :: (Fractional f) => Value f Potential (UnitCons Volt POne UnitNil)
volt = one

-- |One farad.
farad :: (Fractional f) => Value f Capacitance (UnitCons Farad POne UnitNil)
farad = one

-- |One ohm.
ohm :: (Fractional f) => Value f Resistance (UnitCons Ohm POne UnitNil)
ohm = one

-- |One siemens.
siemens :: (Fractional f) => Value f Conductance (UnitCons Siemens POne UnitNil)
siemens = one

-- |One weber.
weber :: (Fractional f) => Value f Flux (UnitCons Weber POne UnitNil)
weber = one

-- |One tesla.
tesla :: (Fractional f) => Value f FluxDensity (UnitCons Tesla POne UnitNil)
tesla = one

-- |One henry.
henry :: (Fractional f) => Value f Inductance (UnitCons Henry POne UnitNil)
henry = one