{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators #-}

class (Convertable a b) => Unit a b where
	coerce :: (Unit a c, Convertable a c, Fractional d) => Value d a b -> Value d a c

instance (Unit a b, Unit c d, Convertable (a -*- c) (Mul a b c d)) => Unit (a -*- c) (Mul a b c d) where
	coerce a = let result = Value { val = factor a * val a / factor result, unit = constructor } in result

instance (Unit a b, Unit c d, Convertable (a -/- c) (Div a b c d)) => Unit (a -/- c) (Div a b c d) where
	coerce a = let result = Value { val = factor a * val a / factor result, unit = constructor } in result

data a -*- b = (-*-)
data a -/- b = (-/-)

data Mul a b c d = Mul {
	mul_l :: (Unit a b) => b,
	mul_r :: (Unit c d) => d
}

data Div a b c d = Div {
	div_l :: (Unit a b) => b,
	div_r :: (Unit c d) => d
}

data Value a b c = Value {
	val :: a,
	unit :: (Unit b c) => c
}

instance (Unit a b, Unit c d, Show b, Show d) => Show (Mul a b c d) where
	show m = (show $ mul_l m) ++ "*" ++ (show $ mul_r m)

instance (Unit a b, Unit c d, Show b, Show d) => Show (Div a b c d) where
	show m = (show $ div_l m) ++ "/(" ++ (show $ div_r m) ++ ")"

instance (Unit b c, Show a, Show c) => Show (Value a b c) where
	show u = (show $ val u) ++ " " ++ (show $ unit u)

class Convertable a b | b -> a where
	factor :: (Fractional f) => Value f a b -> f
	one :: (Fractional f) => Value f a b
	one = Value { val = 1, unit = constructor }
	constructor :: b


instance (Convertable a b, Convertable c d) => Convertable (a -*- c) (Mul a b c d) where
	factor :: (Fractional f) => Value f (a -*- c) (Mul a b c d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in factor left * factor right
	one = Value { val = 1, unit = constructor }
	constructor :: Mul a b c d
	constructor = Mul constructor constructor

instance (Convertable a b, Convertable c d) => Convertable (a -/- c) (Div a b c d) where
	factor :: (Fractional f) => Value f (a -/- c) (Div a b c d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in factor left / factor right
	one = Value { val = 1, unit = constructor }
	constructor :: Div a b c d
	constructor = Div constructor constructor


--

data Length = Length

data Meter = Meter

type Meters = (Fractional f) => Value f Length Meter
type Kilometers = (Fractional f) => Value f Length (Kilo Meter)
type Kmph = (Fractional f) => Value f (Length -/- Time) (Div Length (Kilo Meter) Time Hour)

instance Show Meter where
	show _ = "m"

data Mile = Mile

type Miles = (Fractional f) => Value f Length Mile

instance Show Mile where
	show _ = "mile"

data Inch = Inch

type Inches = (Fractional f) => Value f Length Inch

instance Show Inch where
	show _ = "in"

instance Convertable Length Mile where
	factor = const 1609
	constructor = Mile

instance Convertable Length Meter where
	factor = const 1
	constructor = Meter

instance Convertable Length Inch where
	factor = const 0.0254
	constructor = Inch

--

data Time = Time

data Second = Second

type Seconds = (Fractional f) => Value f Time Second

instance Show Second where
	show _ = "s"

instance Convertable Time Second where
	factor _ = 1
	constructor = Second

data Hour = Hour
type Hours = (Fractional f) => Value f Time Hour

instance Show Hour where
	show _ = "h"

instance Convertable Time Hour where
	factor _ = 3600
	constructor = Hour

data Minute = Minute
type Minutes = (Fractional f) => Value f Time Minute

instance Show Minute where
	show _ = "min."

instance Convertable Time Minute where
	factor _ = 60
	constructor = Minute

data Day = Day
type Days = (Fractional f) => Value f Time Day

instance Show Day where
	show _ = "day"

instance Convertable Time Day where
	factor _ = 60 * 60 * 24
	constructor = Day

--

data Kilo a = Kilo a

instance (Show a) => Show (Kilo a) where
	show (Kilo u) = "k" ++ (show u)

instance (Unit a b, Convertable a b) => Convertable a (Kilo b) where
	factor :: (Fractional f) => Value f a (Kilo b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	           in 1000 * (factor sub)
	one :: (Fractional f) => Value f a (Kilo b)
	one = Value {val = 1, unit = constructor }
		where u :: Value Double a b
		      u = one
	constructor :: Kilo b
	constructor = Kilo (constructor :: b)

--

instance (Convertable Length a) => Unit Length a where
	coerce :: (Unit Length c, Convertable Length c, Fractional f) => Value f Length a -> Value f Length c
	coerce u = let result = Value {val = factor u * val u / factor result, unit = constructor} in result

instance (Convertable Time a) => Unit Time a where
	coerce :: (Unit Time c, Convertable Time c, Fractional f) => Value f Time a -> Value f Time c
	coerce u = let result = Value {val = factor u * val u / factor result, unit = constructor} in result

--

(.*.) :: (Fractional f, Unit a b, Unit c d, Convertable a b, Convertable c d) => Value f a b -> Value f c d -> Value f (a -*- c) (Mul a b c d)
a .*. b = Value { val = val a * val b, unit = constructor }

(./.) :: (Fractional f, Unit a b, Unit c d, Convertable a b, Convertable c d) => Value f a b -> Value f c d -> Value f (a -/- c) (Div a b c d)
a ./. b = Value { val = val a / val b, unit = constructor }


(.+.) :: (Fractional a, Unit b c, Unit b d, Convertable b d, Convertable b c) => Value a b c -> Value a b d -> Value a b c
a .+. b = Value { val = f a (coerce b), unit = constructor }
	where
		f :: (Fractional a) => Value a b c -> Value a b c -> a
		f a b = val a + val b

meter :: Meters
meter = one

kilometer :: Kilometers
kilometer = one

mile :: Miles
mile = one

inch :: Inches
inch = one

--

second :: Seconds
second = one

minute :: Minutes
minute = one

hour :: Hours
hour = one

day :: Days
day = one