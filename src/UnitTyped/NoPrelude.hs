{-# LANGUAGE DataKinds #-}
-- |A module renaming operators on values to names that would normally collide with the "Prelude".
-- E.g. '+', '*', 'sin'.
-- 
-- Also adds instances for 'Num' that makes it possible to write:
-- 
-- >>> 1 meter / second
module UnitTyped.NoPrelude ((*), (/), (+), (-),
	sin, cos, tan,
	asin, acos, atan,

	(==), (<=), (<), (>=), (>),
	abs, floor, diff

) where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived.Count

import qualified Prelude
import Prelude (Show(..), Fractional, Floating, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..), error)

infixl 6 +, -
infixl 7 *, /

-- |See '.*.'
(*) :: (Fractional f, Convertible a b, Convertible c d, UnitMerge a c u) => Value f a b -> Value f c d -> Value f u (Mul b d)
(*) = (.*.)

-- |See './.'
(/) :: (Fractional f, Convertible a b, Convertible c d, UnitMerge a c' u, UnitNeg c c') => Value f a b -> Value f c d -> Value f u (Div b d)
(/) = (./.)

-- |See '.+.'
(+) :: (Fractional f, Convertible a b, Convertible c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
(+) = (.+.)

-- |See '.-.'
(-) :: (Fractional f, Convertible a b, Convertible c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
(-) = (.-.)

instance (Fractional f, Convertible a b, t ~ Value f a b) => (Prelude.Num (Value f a b -> t)) where
	fromInteger i x = Prelude.fromInteger i ~> x
	(+) = error "This should not happen"
	(*) = error "This should not happen"
	abs = error "This should not happen"
	signum = error "This should not happen"

instance (Fractional f, Convertible a b, t ~ Value f a b) => (Fractional (Value f a b -> t)) where
	fromRational r x = Prelude.fromRational r ~> x

instance (Fractional f, Convertible a (m b), c ~ (Value f a b -> Value f a (m b))) => (Prelude.Num ((Value f a b -> Value f a (m b)) -> c)) where
	fromInteger i x y = Prelude.fromInteger i ~> x y
	(+) = error "This should not happen"
	(*) = error "This should not happen"
	abs = error "This should not happen"
	signum = error "This should not happen"

instance (Fractional f, Convertible a (m b), c ~ (Value f a b -> Value f a (m b))) => (Fractional ((Value f a b -> Value f a (m b)) -> c)) where
	fromRational i x y = Prelude.fromRational i ~> x y

wrap1 :: (Floating f, Convertible NoDimension b) => (f -> f) -> Value f NoDimension b -> Value f NoDimension Count
wrap1 op v = op (val $ coerce v rad) ~> one

sin, cos, tan :: (Floating f, Convertible NoDimension b) => Value f NoDimension b -> Value f NoDimension Count
-- |Calculate the sinus of a value. Works on 'Degree' and 'Radian'.
sin = wrap1 Prelude.sin
-- |Calculate the cosinus of a value. Works on 'Degree' and 'Radian'.
cos = wrap1 Prelude.cos
-- |Calculate the tangens of a value. Works on 'Degree' and 'Radian'.
tan = wrap1 Prelude.tan

wrap2 :: (Floating f) => (f -> f) -> Value f NoDimension Count -> Value f NoDimension Radian
wrap2 op v = op (val v) ~> one

asin, acos, atan :: (Floating f) => Value f NoDimension Count -> Value f NoDimension Radian
-- |Calculate the arcsinus of a value. Always computes 'Radian's.
asin = wrap2 Prelude.asin
-- |Calculate the arccosinus of a value. Always computes 'Radian's.
acos = wrap2 Prelude.acos
-- |Calculate the arctangens of a value. Always computes 'Radian's.
atan = wrap2 Prelude.atan

infixl 5 ==, <, <=, >, >=

(==), (<), (<=), (>), (>=) :: (Convertible a b, Convertible c d, UnitEq c a 'True) => Value Prelude.Rational a b -> Value Prelude.Rational c d -> Bool
-- |See '.==.'
(==) = (.==.)
-- |See '.<.'
(<) = (.<.)
-- |See '.<=.'
(<=) = (.<=.)
-- |See '.>.'
(>) = (.>.)
-- |See '.>.'
(>=) = (.>=.)

-- |Obtain the 'Prelude.floor' of a value.
floor :: (Prelude.RealFrac f) => Value f a b -> Value f a b
floor = mapVal (Prelude.fromInteger . Prelude.floor)

-- |Obtain @x - floor x@ of a value.
diff :: (Convertible a b, Prelude.RealFrac f) => Value f a b -> Value f a b
diff x = x .-. (floor x)

-- |Obtain the 'Prelude.abs' of a value.
abs :: (Fractional f) => Value f a b -> Value f a b
abs = mapVal Prelude.abs