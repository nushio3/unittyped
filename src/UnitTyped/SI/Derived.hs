-- |A module with dimensions and units derived from combining SI units.
module UnitTyped.SI.Derived where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived.Time
import UnitTyped.SI.Meta

import Data.Ratio

-- |Speed. @Length^1 Time^-1@.
type Speed = UnitCons Time NOne (UnitCons Length POne UnitNil)
-- |Acceleration. @Length^1 Time^-2@.
type Acceleration = UnitCons Time NTwo (UnitCons Length POne UnitNil)

-- |Derived unit of speed (kn).
data Knot

instance Convertable Speed Knot where
	factor _ = 1852 / 3600
	showunit _ _ = "kn"

--

-- |Force. @Length^1 Time^-1 Mass^1@.
type Force = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length POne UnitNil))
-- |Unit of force (N).
data Newton

instance Convertable Force Newton where
	factor _ = 1
	showunit _ _ = "N"

--

-- |Energy. @Length^2 Time^-2 Mass^1@.
type Energy = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length PTwo UnitNil))

-- |Unit of energy (J).
data Joule

instance Convertable Energy Joule where
	factor _ = 1
	showunit _ _ = "J"

-- |Unit of energy (eV).
data Ev

instance Convertable Energy Ev where
	factor _ = 1.60217656535e-19
	showunit _ _ = "eV"

--

-- |Energy. @Length^2 Time^-3 Mass^1@.
type Power = UnitCons Time NThree (UnitCons Length PTwo (UnitCons Mass POne UnitNil))

-- |Unit of power (W).
data Watt

instance Convertable Power Watt where
	factor _ = 1
	showunit _ _ = "W"

--

-- |Energy. @Length^-1 Time^-2 Mass^1@.
type Pressure = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length NOne UnitNil))

-- |Unit of pressure (Pa).
data Pascal

instance Convertable Pressure Pascal where
	factor _ = 1
	showunit _ _ = "Pa"

-- |Unit of pressure (bar).
data Bar

instance Convertable Pressure Bar where
	factor _ = 1e5
	showunit _ _ = "bar"

-- |Unit of pressure (mmHg).
data MmHg

instance Convertable Pressure MmHg where
	factor _ = 133.322
	showunit _ _ = "mmHg"

--

-- |Electric charge. @Time^1 Current^1@.
type Charge = (UnitCons Time POne (UnitCons Current POne UnitNil))

-- |Unit of chage (C).
data Coulomb

instance Convertable Charge Coulomb where
	factor _ = 1
	showunit _ _ = "C"

--

-- |Electric potential. @Time^-3 Current^-1 Mass^1 Length^2@.
type Potential = (UnitCons Current NOne (UnitCons Mass POne (UnitCons Length PTwo (UnitCons Time NThree UnitNil))))

-- |Unit of potential (V).
data Volt

instance Convertable Potential Volt where
	factor _ = 1
	showunit _ _ = "V"

--

-- |Electric capacitance. @Current^2 Mass^-1 Length^2 Time^4@.
type Capacitance = (UnitCons Current PTwo (UnitCons Mass NOne (UnitCons Length NTwo (UnitCons Time PFour UnitNil))))

-- |Unit of capacitance (F).
data Farad

instance Convertable Capacitance Farad where
	factor _ = 1
	showunit _ _ = "F"

--

-- |Electric resistance. @Current^-2 Time^-3 Length^2 Mass^1@.
type Resistance = (UnitCons Current NTwo (UnitCons Time NThree (UnitCons Length PTwo (UnitCons Mass POne UnitNil))))

-- |Unit of resistance (Ω).
data Ohm

instance Convertable Resistance Ohm where
	factor _ = 1
	showunit _ _ = "Ω"

--

-- |Electric conductance. @Current^2 Mass^-1 Length^-2 Time^3@.
type Conductance = (UnitCons Current PTwo (UnitCons Mass NOne (UnitCons Length NTwo (UnitCons Time PThree UnitNil))))

-- |Unit of conductance (S).
data Siemens

instance Convertable Conductance Siemens where
	factor _ = 1
	showunit _ _ = "S"

--

-- |Magnetic flux. @Current^-1 Length^2 Mass^1 Time^-2@.
type Flux = (UnitCons Current NOne (UnitCons Length PTwo (UnitCons Mass POne (UnitCons Time NTwo UnitNil))))

-- |Unit of magnetic flux (Wb).
data Weber

instance Convertable Flux Weber where
	factor _ = 1
	showunit _ _ = "Wb"

--

-- |Magnetic field strength. @Time^-2 Mass^1 Current^-1@.
type FluxDensity = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Current NOne UnitNil))

-- |Unit of magnetic field strength (T).
data Tesla

instance Convertable FluxDensity Tesla where
	factor _ = 1
	showunit _ _ = "T"

--

-- |Inductance. @Current^-2 Time^-2 Mass^1 Length^2@.
type Inductance = (UnitCons Current NTwo (UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length PTwo UnitNil))))

-- |Unit of Inductance (H).
data Henry

instance Convertable Inductance Henry where
	factor _ = 1
	showunit _ _ = "H"

--

-- |One knot.
knot :: (Fractional f) => Value f Speed Knot
knot = one

-- |One newton.
newton :: (Fractional f) => Value f Force Newton
newton = one

-- |One joule.
joule :: (Fractional f) => Value f Energy Joule
joule = one

-- |One eV.
eV :: (Fractional f) => Value f Energy Ev
eV = one

-- |One kwh.
kwh :: (Fractional f) => Value f Energy (Mul (Kilo Watt) Hour)
kwh = one

-- |One watt.
watt :: (Fractional f) => Value f Power Watt
watt = one

-- |One pascal.
pascal :: (Fractional f) => Value f Pressure Pascal
pascal = one

-- |One bar.
bar :: (Fractional f) => Value f Pressure Bar
bar = one

-- |One mmHg.
mmHg :: (Fractional f) => Value f Pressure MmHg
mmHg = one

-- |One coulomb.
coulomb :: (Fractional f) => Value f Charge Coulomb
coulomb = one

-- |One volt.
volt :: (Fractional f) => Value f Potential Volt
volt = one

-- |One farad.
farad :: (Fractional f) => Value f Capacitance Farad
farad = one

-- |One ohm.
ohm :: (Fractional f) => Value f Resistance Ohm
ohm = one

-- |One siemens.
siemens :: (Fractional f) => Value f Conductance Siemens
siemens = one

-- |One weber.
weber :: (Fractional f) => Value f Flux Weber
weber = one

-- |One tesla.
tesla :: (Fractional f) => Value f FluxDensity Tesla
tesla = one

-- |One henry.
henry :: (Fractional f) => Value f Inductance Henry
henry = one