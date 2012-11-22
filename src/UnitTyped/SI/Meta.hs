module UnitTyped.SI.Meta where

import UnitTyped.SI
import UnitTyped

----
-- Meta-units
----

class (Convertable a b) => MetaUnit (m :: * -> *) a b where
	metafactor :: (Fractional f) => Value f a (m b) -> f
	metashow :: (Fractional f) => Value f a (m b) -> String

instance (MetaUnit m a b, Convertable a b) => Convertable a (m b) where
	factor :: (Fractional f) => Value f a (m b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	               self :: (Fractional f) => Value f a (m b)
	               self = one
	           in (metafactor self) * (factor sub)
	showunit b v = let
			 		   u :: (Fractional f) => Value f a b
					   u = one
				   in metashow v ++ (showunit False u) 

--


data Deca a

instance (Convertable a b) => MetaUnit Deca a b where
	metafactor _ = 10
	metashow _ = "da"


data Hecto a

instance (Convertable a b) => MetaUnit Hecto a b where
	metafactor _ = 10^2
	metashow _ = "h"


data Kilo a

instance (Convertable a b) => MetaUnit Kilo a b where
	metafactor _ = 10^3
	metashow _ = "k"


data Mega a

instance (Convertable a b) => MetaUnit Mega a b where
	metafactor _ = 10^6
	metashow _ = "M"


data Giga a

instance (Convertable a b) => MetaUnit Giga a b where
	metafactor _ = 10^9
	metashow _ = "G"


data Tera a

instance (Convertable a b) => MetaUnit Tera a b where
	metafactor _ = 10^12
	metashow _ = "T"


data Peta a

instance (Convertable a b) => MetaUnit Peta a b where
	metafactor _ = 10^15
	metashow _ = "P"


data Exa a

instance (Convertable a b) => MetaUnit Exa a b where
	metafactor _ = 10^18
	metashow _ = "E"


data Zetta a

instance (Convertable a b) => MetaUnit Zetta a b where
	metafactor _ = 10^21
	metashow _ = "Z"


data Yotta a

instance (Convertable a b) => MetaUnit Yotta a b where
	metafactor _ = 10^24
	metashow _ = "Y"


data Deci a

instance (Convertable a b) => MetaUnit Deci a b where
	metafactor _ = 0.1
	metashow _ = "d"


data Centi a

instance (Convertable a b) => MetaUnit Centi a b where
	metafactor _ = 0.1^2
	metashow _ = "c"


data Mili a

instance (Convertable a b) => MetaUnit Mili a b where
	metafactor _ = 0.1^3
	metashow _ = "m"


data Micro a

instance (Convertable a b) => MetaUnit Micro a b where
	metafactor _ = 0.1^6
	metashow _ = "µ"


data Nano a

instance (Convertable a b) => MetaUnit Nano a b where
	metafactor _ = 0.1^9
	metashow _ = "n"


data Pico a

instance (Convertable a b) => MetaUnit Pico a b where
	metafactor _ = 0.1^12
	metashow _ = "p"


data Femto a

instance (Convertable a b) => MetaUnit Femto a b where
	metafactor _ = 0.1^15
	metashow _ = "f"


data Atto a

instance (Convertable a b) => MetaUnit Atto a b where
	metafactor _ = 0.1^18
	metashow _ = "a"


data Zepto a

instance (Convertable a b) => MetaUnit Zepto a b where
	metafactor _ = 0.1^21
	metashow _ = "z"


data Yocto a

instance (Convertable a b) => MetaUnit Yocto a b where
	metafactor _ = 0.1^24
	metashow _ = "y"

------
---- 2 powers
------


data Kibi a

instance (Convertable a b) => MetaUnit Kibi a b where
	metafactor _ = 2 ^ 10
	metashow _ = "Ki"


data Mebi a

instance (Convertable a b) => MetaUnit Mebi a b where
	metafactor _ = 2 ^ 20
	metashow _ = "Mi"


data Gibi a

instance (Convertable a b) => MetaUnit Gibi a b where
	metafactor _ = 2 ^ 30
	metashow _ = "Gi"


data Tebi a

instance (Convertable a b) => MetaUnit Tebi a b where
	metafactor _ = 2 ^ 40
	metashow _ = "Ti"


data Pebi a

instance (Convertable a b) => MetaUnit Pebi a b where
	metafactor _ = 2 ^ 50
	metashow _ = "Pi"


data Exbi a

instance (Convertable a b) => MetaUnit Exbi a b where
	metafactor _ = 2 ^ 60
	metashow _ = "Ei"


data Zebi a

instance (Convertable a b) => MetaUnit Zebi a b where
	metafactor _ = 2 ^ 70
	metashow _ = "Zi"


data Yobi a

instance (Convertable a b) => MetaUnit Yobi a b where
	metafactor _ = 2 ^ 80
	metashow _ = "Yi"


----

deca :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Deca b)
deca _ = one

hecto :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Hecto b)
hecto _ = one

kilo :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Kilo b)
kilo _ = one

mega :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Mega b)
mega _ = one

giga :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Giga b)
giga _ = one

tera :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Tera b)
tera _ = one

peta :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Peta b)
peta _ = one

exa :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Exa b)
exa _ = one

zetta :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Zetta b)
zetta _ = one

yotta :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Yotta b)
yotta _ = one

deci :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Deci b)
deci _ = one

centi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Centi b)
centi _ = one

mili :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Mili b)
mili _ = one

micro :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Micro b)
micro _ = one

nano :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Nano b)
nano _ = one

pico :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Pico b)
pico _ = one

femto :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Femto b)
femto _ = one

atto :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Atto b)
atto _ = one

zepto :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Zepto b)
zepto _ = one

yocto :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Yocto b)
yocto _ = one

--

kibi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Kibi b)
kibi _ = one

mebi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Mebi b)
mebi _ = one

gibi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Gibi b)
gibi _ = one

tebi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Tebi b)
tebi _ = one

pebi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Pebi b)
pebi _ = one

exbi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Exbi b)
exbi _ = one

zebi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Zebi b)
zebi _ = one

yobi :: (Convertable a b, Fractional f) => Value f a b -> Value f a (Yobi b)
yobi _ = one