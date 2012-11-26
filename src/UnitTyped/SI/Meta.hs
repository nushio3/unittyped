{-# LANGUAGE DataKinds #-}
-- |This module creates the SI prefixes for units.
-- This module allows syntax like:
-- 
-- >>> 42 kilo meter
-- 42.0 km
module UnitTyped.SI.Meta (
	Deca, Hecto, Kilo, Mega, Giga, Tera, Peta, Exa, Zetta, Yotta,
	Deci, Centi, Mili, Micro, Nano, Pico, Femto, Atto, Zepto, Yocto,

	deca, hecto, kilo, mega, giga, tera, peta, exa, zetta, yotta,
	deci, centi, mili, micro, nano, pico, femto, atto, zepto, yocto,

	Kibi, Mebi, Gibi, Tebi, Pebi, Exbi, Zebi, Yobi,
	kibi, mebi, gibi, tebi, pebi, exbi, zebi, yobi
) where

import UnitTyped.SI
import UnitTyped

----
-- Meta-units
----

class (Convertible a b) => MetaUnit (m :: * -> *) a b where
	metafactor :: (Fractional f) => Value f a (m b) -> f
	metashow :: (Fractional f) => Value f a (m b) -> String

instance (MetaUnit m a b, Convertible a b) => Convertible a (m b) where
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	               self :: (Fractional f) => Value f a (m b)
	               self = one
	           in metafactor self * factor sub
	showunit b v = let
			 		   u :: (Fractional f) => Value f a b
					   u = one
				   in metashow v ++ showunit False u

--

-- |Create a unit 10^1 times an existing unit.
data Deca a

instance (Convertible a b) => MetaUnit Deca a b where
	metafactor _ = 10
	metashow _ = "da"

-- |Create a unit 10^2 times an existing unit.
data Hecto a

instance (Convertible a b) => MetaUnit Hecto a b where
	metafactor _ = 10^2
	metashow _ = "h"

-- |Create a unit 10^3 times an existing unit.
data Kilo a

instance (Convertible a b) => MetaUnit Kilo a b where
	metafactor _ = 10^3
	metashow _ = "k"

-- |Create a unit 10^6 times an existing unit.
data Mega a

instance (Convertible a b) => MetaUnit Mega a b where
	metafactor _ = 10^6
	metashow _ = "M"

-- |Create a unit 10^9 times an existing unit.
data Giga a

instance (Convertible a b) => MetaUnit Giga a b where
	metafactor _ = 10^9
	metashow _ = "G"

-- |Create a unit 10^12 times an existing unit.
data Tera a

instance (Convertible a b) => MetaUnit Tera a b where
	metafactor _ = 10^12
	metashow _ = "T"

-- |Create a unit 10^15 times an existing unit.
data Peta a

instance (Convertible a b) => MetaUnit Peta a b where
	metafactor _ = 10^15
	metashow _ = "P"

-- |Create a unit 10^18 times an existing unit.
data Exa a

instance (Convertible a b) => MetaUnit Exa a b where
	metafactor _ = 10^18
	metashow _ = "E"

-- |Create a unit 10^21 times an existing unit.
data Zetta a

instance (Convertible a b) => MetaUnit Zetta a b where
	metafactor _ = 10^21
	metashow _ = "Z"

-- |Create a unit 10^24 times an existing unit.
data Yotta a

instance (Convertible a b) => MetaUnit Yotta a b where
	metafactor _ = 10^24
	metashow _ = "Y"

-- |Create a unit 10^-1 times an existing unit.
data Deci a

instance (Convertible a b) => MetaUnit Deci a b where
	metafactor _ = 0.1
	metashow _ = "d"

-- |Create a unit 10^-2 times an existing unit.
data Centi a

instance (Convertible a b) => MetaUnit Centi a b where
	metafactor _ = 0.1^2
	metashow _ = "c"

-- |Create a unit 10^-3 times an existing unit.
data Mili a

instance (Convertible a b) => MetaUnit Mili a b where
	metafactor _ = 0.1^3
	metashow _ = "m"

-- |Create a unit 10^-6 times an existing unit.
data Micro a

instance (Convertible a b) => MetaUnit Micro a b where
	metafactor _ = 0.1^6
	metashow _ = "µ"

-- |Create a unit 10^-9 times an existing unit.
data Nano a

instance (Convertible a b) => MetaUnit Nano a b where
	metafactor _ = 0.1^9
	metashow _ = "n"

-- |Create a unit 10^-12 times an existing unit.
data Pico a

instance (Convertible a b) => MetaUnit Pico a b where
	metafactor _ = 0.1^12
	metashow _ = "p"

-- |Create a unit 10^-15 times an existing unit.
data Femto a

instance (Convertible a b) => MetaUnit Femto a b where
	metafactor _ = 0.1^15
	metashow _ = "f"

-- |Create a unit 10^-18 times an existing unit.
data Atto a

instance (Convertible a b) => MetaUnit Atto a b where
	metafactor _ = 0.1^18
	metashow _ = "a"

-- |Create a unit 10^-21 times an existing unit.
data Zepto a

instance (Convertible a b) => MetaUnit Zepto a b where
	metafactor _ = 0.1^21
	metashow _ = "z"

-- |Create a unit 10^-24 times an existing unit.
data Yocto a

instance (Convertible a b) => MetaUnit Yocto a b where
	metafactor _ = 0.1^24
	metashow _ = "y"

------
---- 2 powers
------

-- |Create a unit 2^10 times an existing unit.
data Kibi a

instance (Convertible a b) => MetaUnit Kibi a b where
	metafactor _ = 2 ^ 10
	metashow _ = "Ki"

-- |Create a unit 2^20 times an existing unit.
data Mebi a

instance (Convertible a b) => MetaUnit Mebi a b where
	metafactor _ = 2 ^ 20
	metashow _ = "Mi"

-- |Create a unit 2^30 times an existing unit.
data Gibi a

instance (Convertible a b) => MetaUnit Gibi a b where
	metafactor _ = 2 ^ 30
	metashow _ = "Gi"

-- |Create a unit 2^40 times an existing unit.
data Tebi a

instance (Convertible a b) => MetaUnit Tebi a b where
	metafactor _ = 2 ^ 40
	metashow _ = "Ti"

-- |Create a unit 2^50 times an existing unit.
data Pebi a

instance (Convertible a b) => MetaUnit Pebi a b where
	metafactor _ = 2 ^ 50
	metashow _ = "Pi"

-- |Create a unit 2^60 times an existing unit.
data Exbi a

instance (Convertible a b) => MetaUnit Exbi a b where
	metafactor _ = 2 ^ 60
	metashow _ = "Ei"

-- |Create a unit 2^70 times an existing unit.
data Zebi a

instance (Convertible a b) => MetaUnit Zebi a b where
	metafactor _ = 2 ^ 70
	metashow _ = "Zi"

-- |Create a unit 2^80 times an existing unit.
data Yobi a

instance (Convertible a b) => MetaUnit Yobi a b where
	metafactor _ = 2 ^ 80
	metashow _ = "Yi"


----

-- |Take a unit and return one deca(unit).
deca :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Deca b)
deca _ = one

-- |Take a unit and return one hecto(unit).
hecto :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Hecto b)
hecto _ = one

-- |Take a unit and return one kilo(unit).
kilo :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Kilo b)
kilo _ = one

-- |Take a unit and return one mega(unit).
mega :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Mega b)
mega _ = one

-- |Take a unit and return one giga(unit).
giga :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Giga b)
giga _ = one

-- |Take a unit and return one tera(unit).
tera :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Tera b)
tera _ = one

-- |Take a unit and return one peta(unit).
peta :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Peta b)
peta _ = one

-- |Take a unit and return one exa(unit).
exa :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Exa b)
exa _ = one

-- |Take a unit and return one zetta(unit).
zetta :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Zetta b)
zetta _ = one

-- |Take a unit and return one yotta(unit).
yotta :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Yotta b)
yotta _ = one

-- |Take a unit and return one deci(unit).
deci :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Deci b)
deci _ = one

-- |Take a unit and return one centi(unit).
centi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Centi b)
centi _ = one

-- |Take a unit and return one mili(unit).
mili :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Mili b)
mili _ = one

-- |Take a unit and return one micro(unit).
micro :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Micro b)
micro _ = one

-- |Take a unit and return one nano(unit).
nano :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Nano b)
nano _ = one

-- |Take a unit and return one pico(unit).
pico :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Pico b)
pico _ = one

-- |Take a unit and return one femto(unit).
femto :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Femto b)
femto _ = one

-- |Take a unit and return one atto(unit).
atto :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Atto b)
atto _ = one

-- |Take a unit and return one zepto(unit).
zepto :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Zepto b)
zepto _ = one

-- |Take a unit and return one yocto(unit).
yocto :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Yocto b)
yocto _ = one

--

-- |Take a unit and return one kibi(unit).
kibi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Kibi b)
kibi _ = one

-- |Take a unit and return one mebi(unit).
mebi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Mebi b)
mebi _ = one

-- |Take a unit and return one gibi(unit).
gibi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Gibi b)
gibi _ = one

-- |Take a unit and return one tebi(unit).
tebi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Tebi b)
tebi _ = one

-- |Take a unit and return one pebi(unit).
pebi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Pebi b)
pebi _ = one

-- |Take a unit and return one exbi(unit).
exbi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Exbi b)
exbi _ = one

-- |Take a unit and return one zebi(unit).
zebi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Zebi b)
zebi _ = one

-- |Take a unit and return one yobi(unit).
yobi :: (Convertible a b, Fractional f) => Value f a b -> Value f a (Yobi b)
yobi _ = one
