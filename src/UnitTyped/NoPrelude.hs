module UnitTyped.NoPrelude where

import UnitTyped.Units

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (++), Double, const, Bool(..), otherwise, undefined, String(..), error)

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