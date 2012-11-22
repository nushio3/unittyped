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

class Add (a :: Number) (b :: Number) (sum :: Number) | a b -> sum, a sum -> b where

instance Add Zero b b
instance (Suc' b b') => Add (Pos One) b b'
instance (Pre' b b') => Add (Neg One) b b'
instance (Add (Neg a) b (Neg sum)) => Add (Neg (Suc a)) b (Neg (Suc sum))
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
instance (Add value value' sum) => UnitAppend' () unit value (UnitCons unit value' rest) (UnitCons unit sum rest)
instance (UnitAppend' q unit value rest rest', value'' ~ value') => UnitAppend' q unit value (UnitCons unit' value' rest) (UnitCons unit' value'' rest')

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

instance (Convertable a b, Convertable c d, UnitMerge a c u) => Convertable u (Mul a b c d) where
	factor :: (Fractional f) => Value f u (Mul a b c d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (Prelude.*) (factor left) (factor right)
	constructor :: Mul a b c d
	constructor = Mul constructor constructor

instance (Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Convertable u (Div a b c d) where
	factor :: (Fractional f) => Value f u (Div a b c d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (Prelude./) (factor left) (factor right)
	constructor :: Div a b c d
	constructor = Div constructor constructor

---- We can coerce something of a specific dimension into any other unit in the same dimension

coerce :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d
coerce u = let result = Value {val = (Prelude./) ((Prelude.*) (factor u) (val u)) (factor result), unit = constructor} in result

data Mul a b c d = Mul {
	mul_l :: (Convertable a b) => b,
	mul_r :: (Convertable c d) => d
}

data Div a b c d = Div {
	div_l :: (Convertable a b) => b,
	div_r :: (Convertable c d) => d
}

data Value a (b :: UnitMap) c = Value {
	val :: a,
	unit :: (Convertable b c) => c
}

instance (Convertable a b, Convertable c d, Show b, Show d) => Show (Mul a b c d) where
	show m = (show $ mul_l m) ++ "*" ++ (show $ mul_r m)

instance (Convertable a b, Convertable c d, Show b, Show d) => Show (Div a b c d) where
	show m = (show $ div_l m) ++ "/(" ++ (show $ div_r m) ++ ")"

instance (Convertable b c, Show a, Show c) => Show (Value a b c) where
	show u = (show $ val u) ++ " " ++ (show $ unit u)

-- We currently have 3 operators on values-with-units: division, multiplication and addition

(*) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c u) => Value f a b -> Value f c d -> Value f u (Mul a b c d)
a * b = Value { val = (Prelude.*) (val a) (val b), unit = constructor }

(/) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Value f a b -> Value f c d -> Value f u (Div a b c d)
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
	factor = const 1
	constructor = Meter

--

data Mile = Mile

type Miles = (Fractional f) => Value f LengthUnit Mile

instance Show Mile where
	show _ = "mile"

instance Convertable LengthUnit Mile where
	factor = const 1609
	constructor = Mile
--

data Inch = Inch

type Inches = (Fractional f) => Value f LengthUnit Inch

instance Show Inch where
	show _ = "in"

instance Convertable LengthUnit Inch where
	factor = const 0.0254
	constructor = Inch

--

data Yard = Yard

type Yards = (Fractional f) => Value f LengthUnit Yard

instance Show Yard where
	show _ = "yd"

instance Convertable LengthUnit Yard where
	factor = const 0.9144
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

data Kilo a = Kilo a

instance (Show a) => Show (Kilo a) where
	show (Kilo u) = "k" ++ (show u)

instance (Convertable a b) => Convertable a (Kilo b) where
	factor :: (Fractional f) => Value f a (Kilo b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	           in (Prelude.*) 1000 (factor sub)
	constructor :: Kilo b
	constructor = Kilo (constructor :: b)

data Mili a = Mili a

instance (Show a) => Show (Mili a) where
	show (Mili u) = "m" ++ (show u)

instance (Convertable a b) => Convertable a (Mili b) where
	factor :: (Fractional f) => Value f a (Mili b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	           in (Prelude./) (factor sub) 1000
	constructor :: Mili b
	constructor = Mili (constructor :: b)

data Kibi a = Kibi a

instance (Show a) => Show (Kibi a) where
	show (Kibi u) = "ki" ++ (show u)

instance (Convertable a b) => Convertable a (Kibi b) where
	factor :: (Fractional f) => Value f a (Kibi b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	           in (Prelude.*) 1024 (factor sub)
	constructor :: Kibi b
	constructor = Kibi (constructor :: b)

data Mebi a = Mebi a

instance (Show a) => Show (Mebi a) where
	show (Mebi u) = "Mi" ++ (show u)

instance (Convertable a b) => Convertable a (Mebi b) where
	factor :: (Fractional f) => Value f a (Mebi b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	           in (Prelude.*) 1048576 (factor sub)
	constructor :: Mebi b
	constructor = Mebi (constructor :: b)

----

mkVal f = Value { val = f, unit = constructor }

meter :: (Fractional f) => f -> Value f LengthUnit Meter
meter = mkVal

kilometer :: (Fractional f) => f -> Value f LengthUnit (Kilo Meter)
kilometer = mkVal

milimeter :: (Fractional f) => f -> Value f LengthUnit (Mili Meter)
milimeter = mkVal

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