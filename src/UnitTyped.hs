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
{-# LANGUAGE InstanceSigs #-}

-- |Module defining values with dimensions and units, and mathematical operations on those.
module UnitTyped (
        Convertible(..), Convertible'(..), U(..),
        Value(..), ValueProxy, ValueProxy', proxy',

        Count,

        Nat(..), Number(..),
        MapMerge, MapEq, MapNeg, MapTimes, MapStrip,
        POne, PTwo, PThree, PFour, PFive, PSix,
        NOne, NTwo, NThree, NFour,

        coerce, as, to, one, mkVal, val, (|*|), (|/|), (|+|), (|-|),
        (*|), (|*), (|/), (/|), (|+), (+|), (|-), (-|),
        (|==|), (|<=|), (|<|), (|>=|), (|>|),

        dimension, unit,

        square, cubic
) where

import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable

import qualified Data.Map as M
import Data.Typeable

-- |Type level natural numbers (excluding zero, though).
data Nat = One | Suc Nat

-- |Type level integers.
data Number = Zero | Neg Nat | Pos Nat

-- |Type level +1
type POne = Pos One
-- |Type level +2
type PTwo = Pos (Suc One)
-- |Type level +3
type PThree = Pos (Suc (Suc One))
-- |Type level +4
type PFour = Pos (Suc (Suc (Suc One)))
-- |Type level +5
type PFive = Pos (Suc (Suc (Suc (Suc One))))
-- |Type level +6
type PSix = Pos (Suc (Suc (Suc (Suc (Suc One)))))

-- |Type level -1
type NOne = Neg One
-- |Type level -2
type NTwo = Neg (Suc One)
-- |Type level -3
type NThree = Neg (Suc (Suc One))
-- |Type level -4
type NFour = Neg (Suc (Suc (Suc One)))

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

-- |Remove all 'Zero' values from a map.
class MapStrip (map :: [(*, Number)]) (result :: [(*, Number)]) | map -> result where

instance MapStrip '[] '[]
instance (MapStrip rec rec_result) => MapStrip ('(unit, Zero) ': rec) rec_result
instance (MapStrip rec rec_result) => MapStrip ('(unit, (Pos value)) ': rec) ('(unit, (Pos value)) ': rec_result)
instance (MapStrip rec rec_result) => MapStrip ('(unit, (Neg value)) ': rec) ('(unit, (Neg value)) ': rec_result)

class MapInsert' q unit (value :: Number) (map :: [(*, Number)]) (rest :: [(*, Number)]) | unit value map -> rest where

instance MapInsert' q unit value '[] '[ '(unit, value) ]

instance (Add value value' sum, unit' ~ unit) =>
                 MapInsert' () unit value ( '(unit, value') ': rec) ( '(unit', sum) ': rec)

instance (MapInsert' q unit value rest rest', value'' ~ value', unit'' ~ unit') =>
                 MapInsert' q unit value ( '(unit', value') ': rest) ( '(unit'', value'') ': rest')

class MapInsert unit (value :: Number) (map :: [(*, Number)]) (rest :: [(*, Number)]) where

instance (MapInsert' () unit value map inserted, MapStrip inserted stripped) => MapInsert unit value map stripped where

-- |States that merging the first map with the second map produces the third argument.
-- Merging happens by summing the two values for the same key.
-- Typically, dimensions are merged when multiplicing two values.
class MapMerge (map1 :: [(*, Number)]) (map2 :: [(*, Number)]) (rest :: [(*, Number)]) | map1 map2 -> rest, map1 rest -> map2 where

instance MapMerge '[] map2 map2
instance (MapMerge rest map2 rest2, MapInsert unit value rest2 rec) => MapMerge ('(unit, value) ': rest) map2 rec

-- |Multiply all the values in a map with a specified value.
class MapTimes (value :: Number) (map :: [(*, Number)]) (result :: [(*, Number)]) | value map -> result where

instance MapTimes Zero map '[]
instance MapTimes (Pos One) map map
instance (MapNeg map result) => MapTimes (Neg One) map result
instance (MapTimes (Pos n) map rec, MapMerge map rec result) => MapTimes (Pos (Suc n)) map result
instance (MapTimes (Neg n) map rec, MapNeg map neg_map, MapMerge neg_map rec result) => MapTimes (Neg (Suc n)) map result

-- |States that 'rest' is the same dimension as 'map1', but all integers inverted.
-- Used for division.
class MapNeg (map1 :: [(*, Number)]) (rest :: [(*, Number)]) | map1 -> rest where

instance MapNeg '[] '[]
instance (Negate value value', MapNeg rest rest') => MapNeg ('(unit, value) ': rest) ('(unit, value') ': rest')

class MapNull (map :: [(*, Number)]) (b :: Bool) | map -> b where

instance MapNull '[] True
instance (MapNull rest b', IsZero value b, And b b' result) => MapNull ('(unit, value) ': rest) result

-- |'b' is equal to 'True' if and only if 'map1' and 'map2' represent the same dimension.
class MapEq (map1 :: [(*, Number)]) (map2 :: [(*, Number)]) (b :: Bool) | map1 map2 -> b where

instance MapEq a a True
instance (MapNeg map2 map2', MapMerge map1 map2' sum, MapNull sum b) => MapEq map1 map2 b

-- |A value tagged with its dimension a and unit b.
data Value (a :: [(*, Number)]) (b :: [(*, Number)]) f = Value f
-- |Used as fake argument for the 'Convertible' class.
data ValueProxy (a :: [(*, Number)]) b
-- |Used as fake argument for the 'Convertible'' class.
data ValueProxy' (a :: [(*, Number)]) (b :: [(*, Number)])
-- |Used as fake argument containing a type-level integer.
data NumberProxy (a :: Number)

-- |Obtain a 'ValueProxy'' from a 'Value'.
proxy' :: (Convertible' a b) => Value a b f -> ValueProxy' a b
proxy' _ = error "proxy'"

class FromNumber (a :: Number) where
        fromNumber :: NumberProxy a -> Integer

instance FromNumber Zero where
        fromNumber _ = 0

instance FromNumber (Pos One) where
        fromNumber _ = 1

instance (FromNumber (Pos a)) => FromNumber (Pos (Suc a)) where
        fromNumber _ = 1 + (fromNumber (undefined :: NumberProxy (Pos a)))

instance FromNumber (Neg One) where
        fromNumber _ = -1

instance (FromNumber (Neg a)) => FromNumber (Neg (Suc a)) where
        fromNumber _ = -1 + (fromNumber (undefined :: NumberProxy (Neg a)))

class Dimension a where
    dimension :: Value a b f -> M.Map TypeRep Integer

instance Dimension '[] where
    dimension _ = M.empty

instance (Typeable dim, FromNumber value, Dimension rest) => Dimension ('(dim, value) ': rest) where
    dimension :: forall (b :: [(*, Number)]) f . Value ('(dim, value) ': rest) b f -> M.Map TypeRep Integer
    dimension _ = M.insert (typeOf (error "typeOf" :: dim)) (fromNumber (error "fromNumber" :: NumberProxy value)) (dimension (undefined :: Value rest b f))

class Unit b where
    unit :: Value a b f -> M.Map TypeRep Integer

instance Unit '[] where
    unit _ = M.empty

instance (Typeable uni, FromNumber value, Unit rest) => Unit ('(uni, value) ': rest) where
    unit :: forall (a :: [(*, Number)]) f . Value a ('(uni, value) ': rest) f -> M.Map TypeRep Integer
    unit _ = M.insert (typeOf (error "typeOf" :: uni)) (fromNumber (error "fromNumber" :: NumberProxy value)) (unit (undefined :: Value a rest f))

-- |Convertible is a class that models the fact that the base unit 'b' has dimension 'a'.
class Convertible (a :: [(*, Number)]) b | b -> a where
        -- |The multiplication factor to convert this base unit between other units in the same dimension.
        -- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
        factor :: (Fractional f) => ValueProxy a b -> f
        -- |String representation of a base unit.
        showunit :: ValueProxy a b -> String

-- |Shorthand to create a composed unit containing just one base unit.
type U a = '[ '(a, POne) ]

-- |Convertible' is a class that models the fact that the composed unit 'b' has dimension 'a'.
class Convertible' (a :: [(*, Number)]) (b :: [(*, Number)]) where
        -- |The multiplication factor to convert this unit between other units in the same dimension.
        -- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
        factor' :: (Fractional f) => ValueProxy' a b -> f
        -- |String representation of a unit.
        showunit' :: ValueProxy' a b -> String

instance (MapNull a True) => Convertible' a '[] where
        factor' _ = 1
        showunit' _ = ""

instance (Convertible a b, MapEq a a' True) => Convertible' a' ('(b, POne) ': '[]) where
        factor' _ = factor (undefined :: ValueProxy a b)
        showunit' _ = showunit (undefined :: ValueProxy a b)

instance (FromNumber value, Convertible' rec_dimension rest, MapNeg unit_dimension neg_unit_dimension,
                  MapTimes value neg_unit_dimension times_neg_unit_dimension, MapMerge times_neg_unit_dimension dimension rec_dimension,
                  Convertible unit_dimension unit) => Convertible' dimension ('(unit, value) ': rest) where
        factor' _ = let
                                        rec = factor' (undefined :: ValueProxy' rec_dimension rest)
                                in rec * ((factor (undefined :: ValueProxy unit_dimension unit)) ^^ (fromNumber (undefined :: NumberProxy value)))
        showunit' _ = let
                                        rec = showunit' (undefined :: ValueProxy' rec_dimension rest)
                                        power = fromNumber (undefined :: NumberProxy value)
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

instance (Fractional f, Show f, Convertible' a b) => Show (Value a b f) where
        show u = show (val u) ++ " " ++ showunit' (proxy' u)

-- |coerce something of a specific dimension into any other unit in the same dimension.
-- The second argument is only used for its type, but it allows nice syntax like:
--
-- >>> coerce (120 *| meter / second) (kilo meter / hour)
-- 432.0 km/h
coerce :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c True) => Value a b f -> Value c d f -> Value c d f
coerce u _ = let result = mkVal (factor' (proxy' u) * val u / factor' (proxy' result)) in result

infixl 5 `as`

-- |Shorthand for 'coerce'.
as :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c True) => Value a b f -> Value c d f -> Value c d f
as = coerce

-- |Shorthand for 'flip' 'coerce'
to :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c True) => Value c d f -> Value a b f -> Value c d f
to = flip coerce

infixl 7 |*|, |/|
infixl 6 |+|, |-|

-- |Multiply two values, constructing a value with as dimension the product of the dimensions,
-- and as unit the multplication of the units.
(|*|) :: (Fractional f, Convertible' a b, Convertible' c d, MapMerge a c u, MapMerge b d s) => Value a b f -> Value c d f -> Value u s f
a |*| b = mkVal (val a * val b)

-- |Divide two values, constructing a value with as dimension the division of the dimension of the lhs by the dimension of the rhs,
-- and the same for the units.
(|/|), per :: (Fractional f, Convertible' a b, Convertible' c d, MapMerge a c' u, MapNeg c c', MapNeg d d', MapMerge b d' s) => Value a b f -> Value c d f -> Value u s f
a |/| b = mkVal (val a / val b)
per = (|/|)

-- |Add two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(|+|) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a True) => Value a b f -> Value c d f -> Value a b f
a |+| b = mkVal (val a + val (coerce b a))

-- |Subtract two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(|-|) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a True) => Value a b f -> Value c d f -> Value a b f
a |-| b = mkVal (val a - val (coerce b a))

infixl 9 *|, |*, |/, /|
infixl 8 +|, |+, -|, |-

-- |Multiply a scalar by a unit.
(*|) :: (Convertible' a b, Fractional f) => f -> Value a b f -> Value a b f
d *| u = mkVal (d * val u)

-- |Multiply a unit by a scalar.
(|*) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
u |* d = mkVal (val u * d)

-- |Divide a scalar by a unit.
(/|) :: (Convertible' a b, Fractional f, MapNeg a a', MapNeg b b') => f -> Value a b f -> Value a' b' f
d /| u = mkVal (d / val u)

-- |Divide a unit by a scalar.
(|/) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
u |/ d = mkVal (val u / d)

-- |Add a unit to a scalar.
(+|) :: (Convertible' a b, Fractional f) => f -> Value a b f -> Value a b f
d +| u = mkVal (d + val u)

-- |Add a scalar to a unit.
(|+) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
u |+ d = mkVal (val u + d)

-- |Subtract a unit from a scalar.
(-|) :: (Convertible' a b, Fractional f) => f -> Value a b f -> Value a b f
d -| u = mkVal (d - val u)

-- |Subtract a scalar from a unit.
(|-) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
u |- d = mkVal (val u - d)

-- |Create a new value with given scalar as value.
mkVal :: f -> Value a b f
mkVal = Value

-- |Obtain the value of a value wrapped in a type.
val :: Value a b f -> f
val (Value f) = f

instance (Enum f, RealFrac f, Convertible' a b, Fractional f) => Enum (Value a b f) where
    succ = (+|) 1
    pred = (-|) 1
    toEnum = mkVal . fromInteger . toInteger
    fromEnum = fromInteger . round . val

instance Functor (Value a b) where
    fmap f = mkVal . f . val

instance Monad (Value a b) where
    x >>= f = f $ val x
    return = mkVal

instance Applicative (Value a b) where
    pure = mkVal
    f <*> x = mkVal $ (val f) (val x)

instance (Convertible' a b, Fractional f) => Monoid (Value a b f) where
    mempty = mkVal 0
    mappend = (|+|)

instance Foldable (Value a b) where
    foldMap f x = f $ val x

instance Traversable (Value a b) where
    traverse f x = mkVal <$> (f $ val x)

-- |A wrapped value with scalar value 1.
one :: (Fractional f, Convertible' a b) => Value a b f
one = mkVal 1

-- |Calculate the square of a value. Identical to pow2, reads better on units:
--
-- >>> 100 *| square meter `as` square yard
-- 119.59900463010803 yd⋅yd⋅#
square :: (Fractional f, MapMerge c c u, MapMerge d d s, Convertible' c d) => Value c d f -> Value u s f
square x = x |*| x

-- |Calculate the third power of a value. Identical to pow3, reads better on units:
--
-- >>> 1 *| cubic inch `as` mili liter
-- 16.387063999999995 mL
cubic :: (Fractional f, MapMerge a c u, MapMerge b d s, MapMerge c c a, MapMerge d d b, Convertible' a b, Convertible' c d) => Value c d f -> Value u s f
cubic x = x |*| x |*| x

wrapB :: (Convertible' a b, Convertible' c d, MapEq c a True) => (Rational -> Rational -> Bool) -> Value a b Rational -> Value c d Rational -> Bool
wrapB op a b = op (val a) (val $ coerce b a)

infixl 4 |==|, |<|, |>|, |<=|, |>=|

(|==|), (|<|), (|>|), (|<=|), (|>=|) :: (Convertible' a b, Convertible' c d, MapEq c a True) => Value a b Rational -> Value c d Rational -> Bool
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
