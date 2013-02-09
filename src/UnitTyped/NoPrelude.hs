{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- |A module renaming operators on values to names that would normally collide with the "Prelude".
-- E.g. '+', '*', 'sin'. This allows, for example:
-- 
-- >>> 1 *| meter / second
module UnitTyped.NoPrelude ((*), (/), (+), (-),
	sin, cos, tan,
	asin, acos, atan,

	(==), (<=), (<), (>=), (>),
	abs, floor, diff

) where

import UnitTyped
import UnitTyped.SI
import UnitTyped.SI.Meta
import UnitTyped.SI.Derived.Count

import qualified Prelude
import Prelude (Show(..), Fractional, Floating, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..), error, fmap)

infixl 6 +, -
infixl 7 *, /

-- |See '|*|'
(*) :: (Fractional f, Convertible' a b, Convertible' c d, MapMerge a c u, MapMerge b d s) => Value a b f -> Value c d f -> Value u s f
(*) = (|*|)

-- |See '|/|'
(/) :: (Fractional f, Convertible' a b, Convertible' c d, MapMerge a c' u, MapNeg c c', MapNeg d d', MapMerge b d' s) => Value a b f -> Value c d f -> Value u s f
(/) = (|/|)

-- |See '|+|'
(+) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a) => Value a b f -> Value c d f -> Value a b f
(+) = (|+|)

-- |See '|-|'
(-) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a) => Value a b f -> Value c d f -> Value a b f
(-) = (|-|)

wrap1 :: (Floating f, Convertible' '[] b) => (f -> f) -> Value '[] b f -> Value '[] '[] f
wrap1 op v = op (val $ coerce v rad) *| one

sin, cos, tan :: (Floating f, Convertible' '[] b) => Value '[] b f -> Value '[] '[] f
-- |Calculate the sinus of a value. Works on 'Degree' and 'Radian'.
sin = wrap1 Prelude.sin
-- |Calculate the cosinus of a value. Works on 'Degree' and 'Radian'.
cos = wrap1 Prelude.cos
-- |Calculate the tangens of a value. Works on 'Degree' and 'Radian'.
tan = wrap1 Prelude.tan

wrap2 :: (Floating f) => (f -> f) -> Value '[] '[] f -> Value '[] (U Radian) f
wrap2 op v = op (val v) *| one

asin, acos, atan :: (Floating f) => Value '[] '[] f -> Value '[] (U Radian) f
-- |Calculate the arcsinus of a value. Always computes 'Radian's.
asin = wrap2 Prelude.asin
-- |Calculate the arccosinus of a value. Always computes 'Radian's.
acos = wrap2 Prelude.acos
-- |Calculate the arctangens of a value. Always computes 'Radian's.
atan = wrap2 Prelude.atan

infixl 5 ==, <, <=, >, >=

(==), (<), (<=), (>), (>=) :: (Convertible' a b, Convertible' c d, MapEq c a) => Value a b Prelude.Rational -> Value c d Prelude.Rational -> Bool
-- |See '|==|'
(==) = (|==|)
-- |See '|<|'
(<) = (|<|)
-- |See '|<=|'
(<=) = (|<=|)
-- |See '|>|'
(>) = (|>|)
-- |See '|>|'
(>=) = (|>=|)

-- |Obtain the 'Prelude.floor' of a value.
floor :: (Prelude.RealFrac f) => Value a b f -> Value a b f
floor = fmap (Prelude.fromInteger . Prelude.floor)

-- |Obtain @x - floor x@ of a value.
diff :: (Convertible' a b, Prelude.RealFrac f) => Value a b f -> Value a b f
diff x = x |-| (floor x)

-- |Obtain the 'Prelude.abs' of a value.
abs :: (Fractional f) => Value a b f -> Value a b f
abs = fmap Prelude.abs


-- |Square root of a unit-typed value.
sqrt :: (Floating f, MapMerge c c u, MapMerge d d s, Convertible' c d) => Value u s f -> Value c d f 
sqrt  = mkVal . Prelude.sqrt . val 
{-# INLINE sqrt #-}


