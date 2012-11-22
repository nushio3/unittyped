module UnitTyped.SI.Derived where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived.Time
import UnitTyped.SI.Meta

import Data.Ratio

type Speed = UnitCons Time (Neg One) (UnitCons Length (Pos One) UnitNil)
type Acceleration = UnitCons Time (Neg (Suc One)) (UnitCons Length (Pos One) UnitNil)

--

type Force = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos One) UnitNil))
data Newton

instance Convertable Force Newton where
	factor _ = 1
	showunit _ _ = "N"

--

type Energy = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) UnitNil))

data Joule

instance Convertable Energy Joule where
	factor _ = 1
	showunit _ _ = "J"

data Ev

instance Convertable Energy Ev where
	factor _ = 1.60217656535e-19
	showunit _ _ = "eV"

--

type Power = UnitCons Time (Neg (Suc (Suc One))) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))

data Watt

instance Convertable Power Watt where
	factor _ = 1
	showunit _ _ = "W"

--

type Pressure = UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Neg One) UnitNil))

data Pascal

instance Convertable Pressure Pascal where
	factor _ = 1
	showunit _ _ = "Pa"

--

type Charge = (UnitCons Time (Pos One) (UnitCons Current (Pos One) UnitNil))

data Coulomb

instance Convertable Charge Coulomb where
	factor _ = 1
	showunit _ _ = "C"

--

type Potential = (UnitCons Current (Neg One) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) (UnitCons Time (Neg (Suc (Suc One))) UnitNil))))

data Volt

instance Convertable Potential Volt where
	factor _ = 1
	showunit _ _ = "V"

--

type Capacitance = (UnitCons Current (Pos (Suc One)) (UnitCons Mass (Neg One) (UnitCons Length (Neg (Suc One)) (UnitCons Time (Pos (Suc (Suc (Suc One)))) UnitNil))))

data Farad

instance Convertable Capacitance Farad where
	factor _ = 1
	showunit _ _ = "F"

--

type Resistance = (UnitCons Current (Neg (Suc One)) (UnitCons Time (Neg (Suc (Suc One))) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) UnitNil))))

data Ohm

instance Convertable Resistance Ohm where
	factor _ = 1
	showunit _ _ = "Ω"

--

type Conductance = (UnitCons Current (Pos (Suc One)) (UnitCons Mass (Neg One) (UnitCons Length (Neg (Suc One)) (UnitCons Time (Pos (Suc (Suc One))) UnitNil))))

data Siemens

instance Convertable Conductance Siemens where
	factor _ = 1
	showunit _ _ = "S"

--

type Flux = (UnitCons Current (Neg One) (UnitCons Length (Pos (Suc One)) (UnitCons Mass (Pos One) (UnitCons Time (Neg (Suc One)) UnitNil))))

data Weber

instance Convertable Flux Weber where
	factor _ = 1
	showunit _ _ = "Wb"

--

type FluxDensity = (UnitCons Length (Neg One) (UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Current (Neg One) UnitNil))))

data Tesla

instance Convertable FluxDensity Tesla where
	factor _ = 1
	showunit _ _ = "T"

--

type Inductance = (UnitCons Current (Neg (Suc One)) (UnitCons Time (Neg (Suc One)) (UnitCons Mass (Pos One) (UnitCons Length (Pos (Suc One)) UnitNil))))

data Henry

instance Convertable Inductance Henry where
	factor _ = 1
	showunit _ _ = "H"

--

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