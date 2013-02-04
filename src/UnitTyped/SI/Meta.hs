{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Typeable
import UnitTyped.SI
import UnitTyped

----
-- Meta-units
----

data MetaProxy (m :: * -> *) {-(a :: [(*, Number)])-} b

class (Convertible b) => MetaUnit (m :: * -> *) b where
	metafactor :: (Fractional f) => MetaProxy m b -> f
	metashow :: (Fractional f) => MetaProxy m b -> String

instance (MetaUnit m b, Convertible b) => Convertible (m b) where
	factor _ = metafactor (undefined :: MetaProxy m b) * factor (undefined :: ValueProxy (DimensionOf b) b)
	showunit v = metashow (undefined :: MetaProxy m b) ++ showunit (undefined :: ValueProxy (DimensionOf b) b)
        type DimensionOf (m b) = DimensionOf b
--

-- |Create a unit 10^1 times an existing unit.
data Deca a
	deriving Typeable

instance (Convertible b) => MetaUnit Deca {-a-} b where
	metafactor _ = 10
	metashow _ = "da"

-- |Create a unit 10^2 times an existing unit.
data Hecto a
	deriving Typeable

instance (Convertible b) => MetaUnit Hecto {-a-} b where
	metafactor _ = 10^2
	metashow _ = "h"

-- |Create a unit 10^3 times an existing unit.
data Kilo a
	deriving Typeable

instance (Convertible b) => MetaUnit Kilo {-a-} b where
	metafactor _ = 10^3
	metashow _ = "k"

-- |Create a unit 10^6 times an existing unit.
data Mega a
	deriving Typeable

instance (Convertible b) => MetaUnit Mega {-a-} b where
	metafactor _ = 10^6
	metashow _ = "M"

-- |Create a unit 10^9 times an existing unit.
data Giga a
	deriving Typeable

instance (Convertible b) => MetaUnit Giga {-a-} b where
	metafactor _ = 10^9
	metashow _ = "G"

-- |Create a unit 10^12 times an existing unit.
data Tera a
	deriving Typeable

instance (Convertible b) => MetaUnit Tera {-a-} b where
	metafactor _ = 10^12
	metashow _ = "T"

-- |Create a unit 10^15 times an existing unit.
data Peta a
	deriving Typeable

instance (Convertible b) => MetaUnit Peta {-a-} b where
	metafactor _ = 10^15
	metashow _ = "P"

-- |Create a unit 10^18 times an existing unit.
data Exa a
	deriving Typeable

instance (Convertible b) => MetaUnit Exa {-a-} b where
	metafactor _ = 10^18
	metashow _ = "E"

-- |Create a unit 10^21 times an existing unit.
data Zetta a
	deriving Typeable

instance (Convertible b) => MetaUnit Zetta {-a-} b where
	metafactor _ = 10^21
	metashow _ = "Z"

-- |Create a unit 10^24 times an existing unit.
data Yotta a
	deriving Typeable

instance (Convertible b) => MetaUnit Yotta {-a-} b where
	metafactor _ = 10^24
	metashow _ = "Y"

-- |Create a unit 10^-1 times an existing unit.
data Deci a
	deriving Typeable

instance (Convertible b) => MetaUnit Deci {-a-} b where
	metafactor _ = 0.1
	metashow _ = "d"

-- |Create a unit 10^-2 times an existing unit.
data Centi a
	deriving Typeable

instance (Convertible b) => MetaUnit Centi {-a-} b where
	metafactor _ = 0.1^2
	metashow _ = "c"

-- |Create a unit 10^-3 times an existing unit.
data Mili a
	deriving Typeable

instance (Convertible b) => MetaUnit Mili {-a-} b where
	metafactor _ = 0.1^3
	metashow _ = "m"

-- |Create a unit 10^-6 times an existing unit.
data Micro a
	deriving Typeable

instance (Convertible b) => MetaUnit Micro {-a-} b where
	metafactor _ = 0.1^6
	metashow _ = "µ"

-- |Create a unit 10^-9 times an existing unit.
data Nano a
	deriving Typeable

instance (Convertible b) => MetaUnit Nano {-a-} b where
	metafactor _ = 0.1^9
	metashow _ = "n"

-- |Create a unit 10^-12 times an existing unit.
data Pico a
	deriving Typeable

instance (Convertible b) => MetaUnit Pico {-a-} b where
	metafactor _ = 0.1^12
	metashow _ = "p"

-- |Create a unit 10^-15 times an existing unit.
data Femto a
	deriving Typeable

instance (Convertible b) => MetaUnit Femto {-a-} b where
	metafactor _ = 0.1^15
	metashow _ = "f"

-- |Create a unit 10^-18 times an existing unit.
data Atto a
	deriving Typeable

instance (Convertible b) => MetaUnit Atto {-a-} b where
	metafactor _ = 0.1^18
	metashow _ = "a"

-- |Create a unit 10^-21 times an existing unit.
data Zepto a
	deriving Typeable

instance (Convertible b) => MetaUnit Zepto {-a-} b where
	metafactor _ = 0.1^21
	metashow _ = "z"

-- |Create a unit 10^-24 times an existing unit.
data Yocto a
	deriving Typeable

instance (Convertible b) => MetaUnit Yocto {-a-} b where
	metafactor _ = 0.1^24
	metashow _ = "y"

------
---- 2 powers
------

-- |Create a unit 2^10 times an existing unit.
data Kibi a
	deriving Typeable

instance (Convertible b) => MetaUnit Kibi {-a-} b where
	metafactor _ = 2 ^ 10
	metashow _ = "Ki"

-- |Create a unit 2^20 times an existing unit.
data Mebi a
	deriving Typeable

instance (Convertible b) => MetaUnit Mebi {-a-} b where
	metafactor _ = 2 ^ 20
	metashow _ = "Mi"

-- |Create a unit 2^30 times an existing unit.
data Gibi a
	deriving Typeable

instance (Convertible b) => MetaUnit Gibi {-a-} b where
	metafactor _ = 2 ^ 30
	metashow _ = "Gi"

-- |Create a unit 2^40 times an existing unit.
data Tebi a
	deriving Typeable

instance (Convertible b) => MetaUnit Tebi {-a-} b where
	metafactor _ = 2 ^ 40
	metashow _ = "Ti"

-- |Create a unit 2^50 times an existing unit.
data Pebi a
	deriving Typeable

instance (Convertible b) => MetaUnit Pebi {-a-} b where
	metafactor _ = 2 ^ 50
	metashow _ = "Pi"

-- |Create a unit 2^60 times an existing unit.
data Exbi a
	deriving Typeable

instance (Convertible b) => MetaUnit Exbi {-a-} b where
	metafactor _ = 2 ^ 60
	metashow _ = "Ei"

-- |Create a unit 2^70 times an existing unit.
data Zebi a
	deriving Typeable

instance (Convertible b) => MetaUnit Zebi {-a-} b where
	metafactor _ = 2 ^ 70
	metashow _ = "Zi"

-- |Create a unit 2^80 times an existing unit.
data Yobi a
	deriving Typeable

instance (Convertible b) => MetaUnit Yobi {-a-} b where
	metafactor _ = 2 ^ 80
	metashow _ = "Yi"


----

-- |Take a unit and return one deca(unit).
deca :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Deca b)) f
{-# INLINE deca #-}
deca (Value x) = (Value x)

-- |Take a unit and return one hecto(unit).
hecto :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Hecto b)) f
{-# INLINE hecto #-}
hecto (Value x) = (Value x)

-- |Take a unit and return one kilo(unit).
kilo :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Kilo b)) f
{-# INLINE kilo #-}
kilo (Value x) = (Value x)

-- |Take a unit and return one mega(unit).
mega :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Mega b)) f
mega (Value x) = (Value x)

-- |Take a unit and return one giga(unit).
giga :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Giga b)) f
giga (Value x) = (Value x)

-- |Take a unit and return one tera(unit).
tera :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Tera b)) f
tera (Value x) = (Value x)

-- |Take a unit and return one peta(unit).
peta :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Peta b)) f
peta (Value x) = (Value x)

-- |Take a unit and return one exa(unit).
exa :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Exa b)) f
exa (Value x) = (Value x)

-- |Take a unit and return one zetta(unit).
zetta :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Zetta b)) f
zetta (Value x) = (Value x)

-- |Take a unit and return one yotta(unit).
yotta :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Yotta b)) f
yotta (Value x) = (Value x)

-- |Take a unit and return one deci(unit).
deci :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Deci b)) f
deci (Value x) = (Value x)

-- |Take a unit and return one centi(unit).
centi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Centi b)) f
centi (Value x) = (Value x)

-- |Take a unit and return one mili(unit).
mili :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Mili b)) f
mili (Value x) = (Value x)

-- |Take a unit and return one micro(unit).
micro :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Micro b)) f
micro (Value x) = (Value x)

-- |Take a unit and return one nano(unit).
nano :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Nano b)) f
nano (Value x) = (Value x)

-- |Take a unit and return one pico(unit).
pico :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Pico b)) f
pico (Value x) = (Value x)

-- |Take a unit and return one femto(unit).
femto :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Femto b)) f
femto (Value x) = (Value x)

-- |Take a unit and return one atto(unit).
atto :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Atto b)) f
atto (Value x) = (Value x)

-- |Take a unit and return one zepto(unit).
zepto :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Zepto b)) f
zepto (Value x) = (Value x)

-- |Take a unit and return one yocto(unit).
yocto :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Yocto b)) f
yocto (Value x) = (Value x)

--

-- |Take a unit and return one kibi(unit).
kibi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Kibi b)) f
kibi (Value x) = (Value x)

-- |Take a unit and return one mebi(unit).
mebi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Mebi b)) f
mebi (Value x) = (Value x)

-- |Take a unit and return one gibi(unit).
gibi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Gibi b)) f
gibi (Value x) = (Value x)

-- |Take a unit and return one tebi(unit).
tebi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Tebi b)) f
tebi (Value x) = (Value x)

-- |Take a unit and return one pebi(unit).
pebi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Pebi b)) f
pebi (Value x) = (Value x)

-- |Take a unit and return one exbi(unit).
exbi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Exbi b)) f
exbi (Value x) = (Value x)

-- |Take a unit and return one zebi(unit).
zebi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Zebi b)) f
zebi (Value x) = (Value x)

-- |Take a unit and return one yobi(unit).
yobi :: (Convertible b, Fractional f) => Value a (U b) f -> Value a (U (Yobi b)) f
yobi (Value x) = (Value x)
