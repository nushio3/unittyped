{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..))

-- First, type level naturals, and using those, type level integers

data Nat = One | Suc Nat

data Number = Zero | Neg Nat | Pos Nat

type Two = Pos (Suc One)
type Three = Pos (Suc (Suc One))

-- These two are useful for Add

class Suc' (a :: Number) (b :: Number) | a -> b where

instance Suc' Zero (Pos One)
instance Suc' (Pos a) (Pos (Suc a))
instance Suc' (Neg One) Zero
instance Suc' (Neg (Suc a)) (Neg a)

class Pre' (a :: Number) (b :: Number) | a -> b where

instance Pre' Zero (Neg One)
instance Pre' (Neg a) (Neg (Suc a))
instance Pre' (Pos One) Zero
instance Pre' (Pos (Suc a)) (Pos a)

class Add (a :: Number) (b :: Number) (sum :: Number) | a b -> sum where

instance Add Zero b b
instance (Suc' b b') => Add (Pos One) b b'
instance (Pre' b b') => Add (Neg One) b b'
instance (Add (Neg a) b sum, Pre' sum sump) => Add (Neg (Suc a)) b sump
instance (Add (Pos a) b sum, Suc' sum sump) => Add (Pos (Suc a)) b sump

class Negate (a :: Number) (b :: Number) | a -> b, b -> a where

instance Negate Zero Zero
instance Negate (Pos a) (Neg a)
instance Negate (Neg a) (Pos a)

class IsZero (a :: Number) (b :: Bool) | a -> b where

instance IsZero Zero True
instance IsZero (Pos s) False
instance IsZero (Neg s) False

class And (a :: Bool) (b :: Bool) (c :: Bool) | a b -> c where

instance And True True True
instance And False True False
instance And True False False
instance And False False False

-- Now we create a type level list of [(Unit, Integer)], that act like maps
-- With the option to append values, merge two lists and negate a list

data UnitMap where
	UnitNil :: UnitMap
	UnitCons :: a -> Number -> UnitMap -> UnitMap

class UnitOrd unit (num :: Number) | unit -> num where

instance UnitOrd Length (Pos One)
instance UnitOrd Time Two
instance UnitOrd Data Three

class UnitAppend' q unit (value :: Number) (map :: UnitMap) (rest :: UnitMap) | q unit value map -> rest where

instance UnitAppend' q unit value UnitNil (UnitCons unit value UnitNil)
instance (Add value value' sum) =>
		 UnitAppend' () unit value (UnitCons unit value' rest) (UnitCons unit sum rest)
instance (UnitAppend' q unit value rest rest', value'' ~ value', unit'' ~ unit') =>
		 UnitAppend' q unit value (UnitCons unit' value' rest) (UnitCons unit'' value'' rest')

class UnitAppend unit (value :: Number) (map :: UnitMap) (rest :: UnitMap) | unit value map -> rest where

instance UnitAppend' () unit value map rest => UnitAppend unit value map rest where

class UnitMerge (map1 :: UnitMap) (map2 :: UnitMap) (rest :: UnitMap) | map1 map2 -> rest where

instance UnitMerge UnitNil map2 map2
instance (UnitMerge rest map2 rest2, UnitAppend unit value rest2 rec) => UnitMerge (UnitCons unit value rest) map2 rec

class UnitNeg (map1 :: UnitMap) (rest :: UnitMap) | map1 -> rest where

instance UnitNeg UnitNil UnitNil
instance (Negate value value', UnitNeg rest rest') => UnitNeg (UnitCons unit value rest) (UnitCons unit value' rest')

class UnitNull (map :: UnitMap) (b :: Bool) | map -> b where

instance UnitNull UnitNil True
instance (UnitNull rest b', IsZero value b, And b b' result) => UnitNull (UnitCons unit value rest) result

class UnitEq (map1 :: UnitMap) (map2 :: UnitMap) (b :: Bool) | map1 map2 -> b where

instance (UnitNeg map2 map2', UnitMerge map1 map2' sum, UnitNull sum b) => UnitEq map1 map2 b

----

---- Convertable a b means "b is a unit for dimension a"


one :: (Fractional f, Convertable a b) => Value f a b
one = Value { val = 1, unit = constructor }

class Convertable a b | b -> a where
	factor :: (Fractional f) => Value f a b -> f
	constructor :: b

instance (Convertable a b, Convertable c d, UnitMerge a c u) => Convertable u (Mul b d) where
	factor :: (Fractional f) => Value f u (Mul b d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (Prelude.*) (factor left) (factor right)
	constructor :: Mul b d
	constructor = Mul constructor constructor

instance (Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Convertable u (Div b d) where
	factor :: (Fractional f) => Value f u (Div b d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (Prelude./) (factor left) (factor right)
	constructor :: Div b d
	constructor = Div constructor constructor

---- We can coerce something of a specific dimension into any other unit in the same dimension

coerce :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d
coerce u = let result = Value {val = (Prelude./) ((Prelude.*) (factor u) (val u)) (factor result), unit = constructor} in result

coerceTo :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
coerceTo u _ = coerce u

data Mul b d = Mul {
	mul_l :: (Convertable a b) => b,
	mul_r :: (Convertable c d) => d
}

data Div b d = Div {
	div_l :: (Convertable a b) => b,
	div_r :: (Convertable c d) => d
}

data Value a (b :: UnitMap) c = Value {
	val :: a,
	unit :: (Convertable b c) => c
}

instance (Convertable a b, Convertable c d, Show b, Show d) => Show (Mul b d) where
	show m = (show $ mul_l m) ++ "*" ++ (show $ mul_r m)

instance (Convertable a b, Convertable c d, Show b, Show d) => Show (Div b d) where
	show m = (show $ div_l m) ++ "/(" ++ (show $ div_r m) ++ ")"

instance (Convertable b c, Show a, Show c) => Show (Value a b c) where
	show u = (show $ val u) ++ " " ++ (show $ unit u)

-- We currently have 3 operators on values-with-units: division, multiplication and addition

(*) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c u) => Value f a b -> Value f c d -> Value f u (Mul b d)
a * b = Value { val = (Prelude.*) (val a) (val b), unit = constructor }

(/) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Value f a b -> Value f c d -> Value f u (Div b d)
a / b = Value { val = (Prelude./) (val a) (val b), unit = constructor }


(+) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a + b = Value { val = f a (coerce b), unit = constructor }
	where
		f :: (Fractional f) => Value f a b -> Value f a b -> f
		f a b = (Prelude.+) (val a) (val b)

(-) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a - b = Value { val = f a (coerce b), unit = constructor }
	where
		f :: (Fractional a) => Value a b c -> Value a b c -> a
		f a b = (Prelude.-) (val a) (val b)


infixl 6 +, -
infixl 7 *, /

----

data Count = Count
type CountUnit = UnitNil

instance Show Count where
	show _ = "#"

instance Convertable CountUnit Count where
	factor _ = 1
	constructor = Count

----

-- Example dimension: lengths

data Length = Length
type LengthUnit = UnitCons Length (Pos One) UnitNil

--

data Meter = Meter

type Meters = (Fractional f) => Value f LengthUnit Meter
type Kilometers = (Fractional f) => Value f LengthUnit (Kilo Meter)
type Milimeters = (Fractional f) => Value f LengthUnit (Mili Meter)

instance Show Meter where
	show _ = "m"

instance Convertable LengthUnit Meter where
	factor _ = 1
	constructor = Meter

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

-- Example dimension: time

data Time = Time
type TimeUnit = UnitCons Time (Pos One) UnitNil

--

data Second = Second

type Seconds = (Fractional f) => Value f TimeUnit Second

instance Show Second where
	show _ = "s"

instance Convertable TimeUnit Second where
	factor _ = 1
	constructor = Second

--

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

data Data = Data
type DataUnit = UnitCons Data (Pos One) UnitNil

data Byte = Byte
type Bytes = (Fractional f) => Value f DataUnit Byte

instance Show Byte where
	show _ = "B"

instance Convertable DataUnit Byte where
	factor _ = 1
	constructor = Byte

----

-- Example meta-dimensions

class MetaUnit (m :: * -> *) where
	metaconstructor :: a -> m a
	metafactor :: (Fractional f) => Value f a (m b) -> f

instance (MetaUnit m, Convertable a b) => Convertable a (m b) where
	factor :: (Fractional f) => Value f a (m b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	               self :: (Fractional f) => Value f a (m b)
	               self = one
	           in (Prelude.*) (metafactor self) (factor sub)
	constructor :: m b
	constructor = metaconstructor (constructor :: b)

--

data Kilo a = Kilo a

instance MetaUnit Kilo where
	metaconstructor = Kilo
	metafactor _ = 1000

instance (Show a) => Show (Kilo a) where
	show (Kilo u) = "k" ++ (show u)

data Mega a = Mega a

instance MetaUnit Mega where
	metaconstructor = Mega
	metafactor _ = 1000000

instance (Show a) => Show (Mega a) where
	show (Mega u) = "M" ++ (show u)

data Mili a = Mili a

instance MetaUnit Mili where
	metaconstructor = Mili
	metafactor _ = 0.001

instance (Show a) => Show (Mili a) where
	show (Mili u) = "m" ++ (show u)

data Kibi a = Kibi a

instance MetaUnit Kibi where
	metaconstructor = Kibi
	metafactor _ = 1024

instance (Show a) => Show (Kibi a) where
	show (Kibi u) = "ki" ++ (show u)

data Mebi a = Mebi a

instance MetaUnit Mebi where
	metaconstructor = Mebi
	metafactor _ = (Prelude.*) 1024 1024

----

-- Mass

data Mass = Mass
type MassUnit = UnitCons Mass (Pos One) UnitNil

data Gram = Gram
type Grams = (Fractional f) => Value f MassUnit Gram
type Kilograms = (Fractional f) => Value f MassUnit (Kilo Gram)

instance Show Gram where
	show _ = "g"

instance Convertable MassUnit Gram where
	factor _ = 1
	constructor = Gram

data PlanckMass = PlanckMass
type PlanckMasses = (Fractional f) => Value f MassUnit PlanckMass

instance Show PlanckMass where
	show _ = "m_P"

instance Convertable MassUnit PlanckMass where
	factor _ = 2.176513e-5
	constructor = PlanckMass

--

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

----

mkVal f = Value { val = f, unit = constructor }

count :: (Fractional f) => f -> Value f CountUnit Count
count = mkVal

kilo :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Kilo b)
kilo func = mkVal

mili :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Mili b)
mili func = mkVal

mega :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Mega b)
mega func = mkVal

--

meter :: (Fractional f) => f -> Value f LengthUnit Meter
meter = mkVal

kilometer = kilo meter
milimeter = mili meter

mile :: (Fractional f) => f -> Value f LengthUnit Mile
mile = mkVal

inch :: (Fractional f) => f -> Value f LengthUnit Inch
inch = mkVal

yard :: (Fractional f) => f -> Value f LengthUnit Yard
yard = mkVal

----

second :: (Fractional f) => f -> Value f TimeUnit Second
second = mkVal

minute :: (Fractional f) => f -> Value f TimeUnit Minute
minute = mkVal

hour :: (Fractional f) => f -> Value f TimeUnit Hour
hour = mkVal

day :: (Fractional f) => f -> Value f TimeUnit Day
day = mkVal

--

byte :: (Fractional f) => f -> Value f DataUnit Byte
byte = mkVal

kibibyte :: (Fractional f) => f -> Value f DataUnit (Kibi Byte)
kibibyte = mkVal

--

gram :: (Fractional f) => f -> Value f MassUnit Gram
gram = mkVal

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