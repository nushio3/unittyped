module UnitTyped.NoPrelude where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Derived.Count

import qualified Prelude
import Prelude (Show(..), Fractional, Floating, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..), error)

infixl 6 +, -
infixl 7 *, /
infixl 8 .

(*) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c u) => Value f a b -> Value f c d -> Value f u (Mul b d)
(*) = (.*.)

(/) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Value f a b -> Value f c d -> Value f u (Div b d)
(/) = (./.)

(+) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
(+) = (.+.)

(-) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
(-) = (.-.)

(.) :: (Convertable a b, Fractional f) => f -> Value f a b -> Value f a b
(.) = (.$.)

instance (Fractional f, Convertable a b, t ~ Value f a b) => (Prelude.Num (Value f a b -> t)) where
	fromInteger i x = (Prelude.fromInteger i) . x
	(+) = error "This should not happen"
	(*) = error "This should not happen"
	abs = error "This should not happen"
	signum = error "This should not happen"

instance (Fractional f, Convertable a b, t ~ Value f a b) => (Fractional (Value f a b -> t)) where
	fromRational r x = (Prelude.fromRational r) . x

instance (Fractional f, Convertable a (m b), c ~ (Value f a b -> Value f a (m b))) => (Prelude.Num ((Value f a b -> Value f a (m b)) -> c)) where
	fromInteger i x y = (Prelude.fromInteger i) . (x y)
	(+) = error "This should not happen"
	(*) = error "This should not happen"
	abs = error "This should not happen"
	signum = error "This should not happen"

instance (Fractional f, Convertable a (m b), c ~ (Value f a b -> Value f a (m b))) => (Fractional ((Value f a b -> Value f a (m b)) -> c)) where
	fromRational i x y = (Prelude.fromRational i) . (x y)

wrap1 :: (Floating f, Convertable CountUnit b) => (f -> f) -> Value f CountUnit b -> Value f CountUnit Count
wrap1 op v = (op $ val $ coerceTo v rad) . one

sin, cos, tan :: (Floating f, Convertable CountUnit b) => Value f CountUnit b -> Value f CountUnit Count
sin = wrap1 Prelude.sin
cos = wrap1 Prelude.cos
tan = wrap1 Prelude.tan

wrap2 :: (Floating f) => (f -> f) -> Value f CountUnit Count -> Value f CountUnit Radian
wrap2 op v = (op $ val v) . one

asin, acos, atan :: (Floating f) => Value f CountUnit Count -> Value f CountUnit Radian
asin = wrap2 Prelude.asin
acos = wrap2 Prelude.acos
atan = wrap2 Prelude.atan

infixl 5 ==, <, <=, >, >=

(==), (<), (<=), (>), (>=) :: (Convertable a b, Convertable c d, UnitEq c a True) => Value Prelude.Rational a b -> Value Prelude.Rational c d -> Bool
(==) = (.==.)
(<) = (.<.)
(<=) = (.<=.)
(>) = (.>.)
(>=) = (.>=.)