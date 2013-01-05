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
{-# LANGUAGE TypeFamilies #-}

-- |Module defining values with dimensions and units, and mathematical operations on those.
module UnitTyped (
        Convertible(..), Convertible'(..), Unit(..),
        Value(..), ValueProxy, ValueProxy', proxy',

        Count,

        Nat(..), Number(..),
        MapMerge, MapEq, MapNeg, MapTimes, MapNull, P, N,
        POne, PTwo, PThree, PFour, PFive, PSix,
        NOne, NTwo, NThree, NFour,

        coerce, as, to, one, mkVal, val, mapVal, (|*|), (|/|), (|+|), (|-|),
        (*|), (|*), (|/), (/|), (|+), (+|), (|-), (-|),
        (|==|), (|<=|), (|<|), (|>=|), (|>|),

        square, cubic
) where

import GHC.TypeLits

-- |Type level integers. I a b represents a - b.
data Number = I Nat Nat

newtype instance Sing (n :: Number) = SNumber (Integer, Integer)

instance (SingI a, SingI b) => SingI (I a b) where
    sing = SNumber (fromSing (sing :: Sing a), fromSing (sing :: Sing b))

instance SingE (KindParam :: OfKind Number) where
    type DemoteRep (KindParam :: OfKind Number) = Integer
    fromSing (SNumber (a, b)) = a - b

type family P (n :: Nat) :: Number
type instance where
    P n = I n 0

type family N (n :: Nat) :: Number
type instance where
    N n = I 0 n

type POne = I 1 0
-- |Type level +2
type PTwo = I 2 0
-- |Type level +3
type PThree = I 3 0
-- |Type level +4
type PFour = I 4 0
-- |Type level +5
type PFive = I 5 0
-- |Type level +6
type PSix = I 6 0

-- |Type level -1
type NOne = I 0 1
-- |Type level -2
type NTwo = I 0 2
-- |Type level -3
type NThree = I 0 3
-- |Type level -4
type NFour = I 0 4

type family Add (a :: Number) (b :: Number) :: Number
type instance where
    Add (I a b) (I c d) = I (a + c) (b + d)

-- (a - b) * (c - d) = (a * c + b * d) - (a * d + b * c)
type family Times (a :: Number) (b :: Number) :: Number
type instance where
    Times (I a b) (I c d) = I ((a * c) + (b * d)) ((a * d) + (b * c))

type family Negate (a :: Number) :: Number
type instance where
    Negate (I a b) = I b a

class NumberEq (a :: Number) (b :: Number) where
-- a - b = c - d => a + d = c + b
instance ((a + d) <= (c + b), (c + b) <= (a + d)) => NumberEq (I a b) (I c d) where

type family MapInsert unit (value :: Number) (map :: [(*, Number)]) :: [(*, Number)]
type instance where
    MapInsert unit value '[] = '[ '(unit, value) ]
    MapInsert unit value ( '(unit, value') ': rec) = ( '(unit, Add value value') ': rec)
    MapInsert unit value ( '(unit', value') ': rest) = ( '(unit', value') ': MapInsert unit value rest)

-- |Merges two maps. Merging happens by summing the two values for the same key.
-- Typically, dimensions are merged when multiplicing two values.
type family MapMerge (map1 :: [(*, Number)]) (map2 :: [(*, Number)]) :: [(*, Number)]
type instance where
    MapMerge '[] map2 = map2
    MapMerge ('(unit, value) ': rest) map2 = MapInsert unit value (MapMerge rest map2)

-- |Multiply all the values in a map with a specified value.
type family MapTimes (value :: Number) (map :: [(*, Number)]) :: [(*, Number)]
type instance where
    MapTimes (I a a) map = '[]
    MapTimes n '[] = '[]
    MapTimes n ('(unit, value) ': rest) = '(unit, Times n value) ': (MapTimes n rest)

-- |Negates all the values in the given map.
type family MapNeg (map1 :: [(*, Number)]) :: [(*, Number)]
type instance where
    MapNeg map = MapTimes NOne map

class MapNull (map :: [(*, Number)]) where

instance MapNull '[] where
instance (MapNull rest) => MapNull ('(unit, I a a) ': rest) where

-- |This constraint holds if and only if 'map1' and 'map2' represent the same dimension.
class MapEq (map1 :: [(*, Number)]) (map2 :: [(*, Number)]) where
instance MapEq a a where
instance (MapNull (MapMerge map1 (MapNeg map2))) => MapEq map1 map2 where

-- |A value tagged with its dimension a and unit b.
data Value f (a :: [(*, Number)]) (b :: [(*, Number)]) = Value f
-- |Used as fake argument for the 'Convertible' class.
data ValueProxy (a :: [(*, Number)]) b
-- |Used as fake argument for the 'Convertible'' class.
data ValueProxy' (a :: [(*, Number)]) (b :: [(*, Number)])

-- |Obtain a 'ValueProxy'' from a 'Value'.
proxy' :: (Convertible' a b) => Value f a b -> ValueProxy' a b
proxy' _ = error "proxy'"

-- |Convertible is a class that models the fact that the base unit 'b' has dimension 'a'.
class Convertible (a :: [(*, Number)]) b | b -> a where
        -- |The multiplication factor to convert this base unit between other units in the same dimension.
        -- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
        factor :: (Fractional f) => ValueProxy a b -> f
        -- |String representation of a base unit.
        showunit :: ValueProxy a b -> String

-- |Shorthand to create a composed unit containing just one base unit.
type Unit a = '[ '(a, P 1) ]

-- |Convertible' is a class that models the fact that the composed unit 'b' has dimension 'a'.
class Convertible' (a :: [(*, Number)]) (b :: [(*, Number)]) where
        -- |The multiplication factor to convert this unit between other units in the same dimension.
        -- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
        factor' :: (Fractional f) => ValueProxy' a b -> f
        -- |String representation of a unit.
        showunit' :: ValueProxy' a b -> String

instance (MapNull a, MapNull b) => Convertible' a b where
        factor' _ = 1
        showunit' _ = ""

instance (SingI value, Convertible' rec_dimension rest,
          neg_unit_dimension ~ MapNeg unit_dimension,
          times_neg_unit_dimension ~ MapTimes value neg_unit_dimension,
          rec_dimension ~ MapMerge times_neg_unit_dimension dimension,
          Convertible unit_dimension unit) => Convertible' dimension ('(unit, value) ': rest) where
        factor' _ = let
                                        rec = factor' (undefined :: ValueProxy' rec_dimension rest)
                                in rec * ((factor (undefined :: ValueProxy unit_dimension unit)) ^^ (fromSing (sing :: Sing value)))
        showunit' _ = let
                                        rec = showunit' (undefined :: ValueProxy' rec_dimension rest)
                                        power = fromSing (sing :: Sing value)
                                  in (if null rec then "" else rec) ++ (if (not $ null rec) && (power /= 0) then "⋅" else "") ++ (if power /= 0 then (showunit (undefined :: ValueProxy a'' unit)) ++ if power /= 1 then map toSuperScript $ show power else "" else "")

toSuperScript '0' = '\8304'
toSuperScript '1' = '\185'
toSuperScript '2' = '\178'
toSuperScript '3' = '\179'
toSuperScript '4' = '\8308'
toSuperScript '5' = '\8309'
toSuperScript '6' = '\8310'
toSuperScript '7' = '\8311'
toSuperScript '8' = '\8312'
toSuperScript '9' = '\8313'
toSuperScript '-' = '\8315'
toSuperScript x = x

instance (Fractional f, Show f, Convertible' a b) => Show (Value f a b) where
        show u = show (val u) ++ " " ++ showunit' (proxy' u)

-- |coerce something of a specific dimension into any other unit in the same dimension.
-- The second argument is only used for its type, but it allows nice syntax like:
--
-- >>> coerce (120 *| meter / second) (kilo meter / hour)
-- 432.0 km/h
coerce :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c) => Value f a b -> Value f c d -> Value f c d
coerce u _ = let result = mkVal (factor' (proxy' u) * val u / factor' (proxy' result)) in result

infixl 5 `as`

-- |Shorthand for 'coerce'.
as :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c) => Value f a b -> Value f c d -> Value f c d
as = coerce

-- |Shorthand for 'flip' 'coerce'
to :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c) => Value f c d -> Value f a b -> Value f c d
to = flip coerce

infixl 7 |*|, |/|
infixl 6 |+|, |-|

-- |Multiply two values, constructing a value with as dimension the product of the dimensions,
-- and as unit the multplication of the units.
(|*|) :: (Fractional f, Convertible' a b, Convertible' c d) => Value f a b -> Value f c d -> Value f (MapMerge a c) (MapMerge b d)
a |*| b = mkVal (val a * val b)

-- |Divide two values, constructing a value with as dimension the division of the dimension of the lhs by the dimension of the rhs,
-- and the same for the units.
(|/|), per :: (Fractional f, Convertible' a b, Convertible' c d) => Value f a b -> Value f c d -> Value f (MapMerge a (MapNeg c)) (MapMerge b (MapNeg d))
a |/| b = mkVal (val a / val b)
per = (|/|)

-- |Add two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(|+|) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a) => Value f a b -> Value f c d -> Value f a b
a |+| b = mkVal (val a + val (coerce b a))

-- |Subtract two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(|-|) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a) => Value f a b -> Value f c d -> Value f a b
a |-| b = mkVal (val a - val (coerce b a))

infixl 9 *|, |*, |/, /|
infixl 8 +|, |+, -|, |-

-- |Multiply a scalar by a unit.
(*|) :: (Convertible' a b, Fractional f) => f -> Value f a b -> Value f a b
d *| u = mkVal (d * val u)

-- |Multiply a unit by a scalar.
(|*) :: (Convertible' a b, Fractional f) => Value f a b -> f -> Value f a b
u |* d = mkVal (val u * d)

-- |Divide a scalar by a unit.
(/|) :: (Convertible' a b, Fractional f) => f -> Value f a b -> Value f (MapNeg a) (MapNeg b)
d /| u = mkVal (d / val u)

-- |Divide a unit by a scalar.
(|/) :: (Convertible' a b, Fractional f) => Value f a b -> f -> Value f a b
u |/ d = mkVal (val u / d)

-- |Add a unit to a scalar.
(+|) :: (Convertible' a b, Fractional f) => f -> Value f a b -> Value f a b
d +| u = mkVal (d + val u)

-- |Add a scalar to a unit.
(|+) :: (Convertible' a b, Fractional f) => Value f a b -> f -> Value f a b
u |+ d = mkVal (val u + d)

-- |Subtract a unit from a scalar.
(-|) :: (Convertible' a b, Fractional f) => f -> Value f a b -> Value f a b
d -| u = mkVal (d - val u)

-- |Subtract a scalar from a unit.
(|-) :: (Convertible' a b, Fractional f) => Value f a b -> f -> Value f a b
u |- d = mkVal (val u - d)

-- |Create a new value with given scalar as value.
mkVal :: (Fractional f) => f -> Value f a b
mkVal = Value

-- |Obtain the value of a value wrapped in a type.
val :: (Fractional f) => Value f a b -> f
val (Value f) = f

-- |Map over values. The 'Fractional' constraints make it impossible to define a 'Functor' instance, therefore this.
mapVal :: (Fractional f, Fractional g) => (f -> g) -> Value f a b -> Value g a b
mapVal f = mkVal . f . val

-- |A wrapped value with scalar value 1.
one :: (Fractional f, Convertible' a b) => Value f a b
one = mkVal 1

-- |Calculate the square of a value. Identical to pow2, reads better on units:
--
-- >>> 100 *| square meter `as` square yard
-- 119.59900463010803 yd⋅yd⋅#
square :: (Fractional f, Convertible' c d) => Value f c d -> Value f (MapMerge c c) (MapMerge d d)
square x = x |*| x

-- |Calculate the third power of a value. Identical to pow3, reads better on units:
--
-- >>> 1 *| cubic inch `as` mili liter
-- 16.387063999999995 mL
cubic :: (Fractional f, Convertible' t t1, Convertible' c d, MapMerge d d ~ t1, MapMerge c c ~ t) => Value f c d -> Value f (MapMerge (MapMerge c c) c) (MapMerge (MapMerge d d) d)
cubic x = x |*| x |*| x

wrapB :: (Convertible' a b, Convertible' c d, MapEq c a) => (Rational -> Rational -> Bool) -> Value Rational a b -> Value Rational c d -> Bool
wrapB op a b = op (val a) (val $ coerce b a)

infixl 4 |==|, |<|, |>|, |<=|, |>=|

(|==|), (|<|), (|>|), (|<=|), (|>=|) :: (Convertible' a b, Convertible' c d, MapEq c a) => Value Rational a b -> Value Rational c d -> Bool
-- |'==' for values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(|==|) = wrapB (==)
-- |'<' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(|<|) = wrapB (<)
-- |'<=' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(|<=|) = wrapB (<=)
-- |'>' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(|>|) = wrapB (>)
-- |'>=' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(|>=|) = wrapB (>=)

----
-- Counting. These are dimension-less values.
----

--

-- |One thing.
data Count

instance Convertible '[] Count where
        factor _ = 1
        showunit _ = "#"
