module UnitTyped.SI.Derived where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived.Time
import UnitTyped.SI.Meta

import Data.Ratio

type Speed = UnitCons Time NOne (UnitCons Length POne UnitNil)
type Acceleration = UnitCons Time NTwo (UnitCons Length POne UnitNil)


data Knot

instance Convertable Speed Knot where
	factor _ = 1852 / 3600
	showunit _ _ = "kn"

--

type Force = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length POne UnitNil))
data Newton

instance Convertable Force Newton where
	factor _ = 1
	showunit _ _ = "N"

--

type Energy = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length PTwo UnitNil))

data Joule

instance Convertable Energy Joule where
	factor _ = 1
	showunit _ _ = "J"

data Ev

instance Convertable Energy Ev where
	factor _ = 1.60217656535e-19
	showunit _ _ = "eV"

--

type Power = UnitCons Time NThree (UnitCons Length PTwo (UnitCons Mass POne UnitNil))

data Watt

instance Convertable Power Watt where
	factor _ = 1
	showunit _ _ = "W"

--

type Pressure = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length NOne UnitNil))

data Pascal

instance Convertable Pressure Pascal where
	factor _ = 1
	showunit _ _ = "Pa"


data Bar

instance Convertable Pressure Bar where
	factor _ = 1e5
	showunit _ _ = "bar"


data MmHg

instance Convertable Pressure MmHg where
	factor _ = 133.322
	showunit _ _ = "mmHg"

--

type Charge = (UnitCons Time POne (UnitCons Current POne UnitNil))

data Coulomb

instance Convertable Charge Coulomb where
	factor _ = 1
	showunit _ _ = "C"

--

type Potential = (UnitCons Current NOne (UnitCons Mass POne (UnitCons Length PTwo (UnitCons Time NThree UnitNil))))

data Volt

instance Convertable Potential Volt where
	factor _ = 1
	showunit _ _ = "V"

--

type Capacitance = (UnitCons Current PTwo (UnitCons Mass NOne (UnitCons Length NTwo (UnitCons Time PFour UnitNil))))

data Farad

instance Convertable Capacitance Farad where
	factor _ = 1
	showunit _ _ = "F"

--

type Resistance = (UnitCons Current NTwo (UnitCons Time NThree (UnitCons Length PTwo (UnitCons Mass POne UnitNil))))

data Ohm

instance Convertable Resistance Ohm where
	factor _ = 1
	showunit _ _ = "Ω"

--

type Conductance = (UnitCons Current PTwo (UnitCons Mass NOne (UnitCons Length NTwo (UnitCons Time PThree UnitNil))))

data Siemens

instance Convertable Conductance Siemens where
	factor _ = 1
	showunit _ _ = "S"

--

type Flux = (UnitCons Current NOne (UnitCons Length PTwo (UnitCons Mass POne (UnitCons Time NTwo UnitNil))))

data Weber

instance Convertable Flux Weber where
	factor _ = 1
	showunit _ _ = "Wb"

--

type FluxDensity = UnitCons Time NTwo (UnitCons Mass POne (UnitCons Current NOne UnitNil))

data Tesla

instance Convertable FluxDensity Tesla where
	factor _ = 1
	showunit _ _ = "T"

--

type Inductance = (UnitCons Current NTwo (UnitCons Time NTwo (UnitCons Mass POne (UnitCons Length PTwo UnitNil))))

data Henry

instance Convertable Inductance Henry where
	factor _ = 1
	showunit _ _ = "H"

--

knot :: (Fractional f) => Value f Speed Knot
knot = one

newton :: (Fractional f) => Value f Force Newton
newton = one

joule :: (Fractional f) => Value f Energy Joule
joule = one

eV :: (Fractional f) => Value f Energy Ev
eV = one

kwh :: (Fractional f) => Value f Energy (Mul (Kilo Watt) Hour)
kwh = one

watt :: (Fractional f) => Value f Power Watt
watt = one

pascal :: (Fractional f) => Value f Pressure Pascal
pascal = one

bar :: (Fractional f) => Value f Pressure Bar
bar = one

mmHg :: (Fractional f) => Value f Pressure MmHg
mmHg = one

coulomb :: (Fractional f) => Value f Charge Coulomb
coulomb = one

volt :: (Fractional f) => Value f Potential Volt
volt = one

farad :: (Fractional f) => Value f Capacitance Farad
farad = one

ohm :: (Fractional f) => Value f Resistance Ohm
ohm = one

siemens :: (Fractional f) => Value f Conductance Siemens
siemens = one

weber :: (Fractional f) => Value f Flux Weber
weber = one

tesla :: (Fractional f) => Value f FluxDensity Tesla
tesla = one

henry :: (Fractional f) => Value f Inductance Henry
henry = one