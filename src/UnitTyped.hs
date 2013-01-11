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
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |Module defining values with dimensions and units, and mathematical operations on those.
module UnitTyped (
        Convertible(..), Convertible'(..), U(..),
        Value(..), ValueProxy, ValueProxy', proxy',

        Count,

        Nat(..), Number(..),
        MapMerge, MapEq, MapNeg, MapTimes, MapNull, P, N,
        POne, PTwo, PThree, PFour, PFive, PSix,
        NOne, NTwo, NThree, NFour,

        coerce, as, to, one, mkVal, val, (|*|), (|/|), (|+|), (|-|),
        (*|), (|*), (|/), (/|), (|+), (+|), (|-), (-|),
        (|==|), (|<=|), (|<|), (|>=|), (|>|),

        dimension, unit,

        square, cubic
) where

import GHC.TypeLits
import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU

import qualified Data.Map as M
import Data.Typeable

-- |Type level integers. I a b represents a - b.
data Number = I Nat Nat

newtype instance Sing (n :: Number) = SNumber (Integer, Integer)

instance (SingI a, SingI b) => SingI (I a b) where
    sing = SNumber (fromSing (sing :: Sing a), fromSing (sing :: Sing b))

instance SingE (KindParam :: OfKind Number) where
    type DemoteRep (KindParam :: OfKind Number) = Integer
    fromSing (SNumber (a, b)) = a - b

type P n = I n 0
type N n = I 0 n

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
newtype Value (a :: [(*, Number)]) (b :: [(*, Number)]) f = Value f
-- |Used as fake argument for the 'Convertible' class.
data ValueProxy (a :: [(*, Number)]) b
-- |Used as fake argument for the 'Convertible'' class.
data ValueProxy' (a :: [(*, Number)]) (b :: [(*, Number)])

-- |Obtain a 'ValueProxy'' from a 'Value'.
proxy' :: (Convertible' a b) => Value a b f -> ValueProxy' a b
proxy' _ = error "proxy'"

class Dimension a where
    dimension :: Value a b f -> M.Map TypeRep Integer

instance Dimension '[] where
    dimension _ = M.empty

instance (Typeable dim, Dimension rest, SingI value) => Dimension ('(dim, value) ': rest) where
    dimension :: forall (b :: [(*, Number)]) f . Value ('(dim, value) ': rest) b f -> M.Map TypeRep Integer
    dimension _ = M.insert (typeOf (error "typeOf" :: dim)) (fromSing (sing :: Sing value)) (dimension (undefined :: Value rest b f))

class Unit b where
    unit :: Value a b f -> M.Map TypeRep Integer

instance Unit '[] where
    unit _ = M.empty

instance (Typeable uni, Unit rest, SingI value) => Unit ('(uni, value) ': rest) where
    unit :: forall (a :: [(*, Number)]) f . Value a ('(uni, value) ': rest) f -> M.Map TypeRep Integer
    unit _ = M.insert (typeOf (error "typeOf" :: uni)) (fromSing (sing :: Sing value)) (unit (undefined :: Value a rest f))

-- |Convertible is a class that models the fact that the base unit 'b' has dimension 'a'.
class Convertible (a :: [(*, Number)]) b | b -> a where
        -- |The multiplication factor to convert this base unit between other units in the same dimension.
        -- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
        factor :: (Fractional f) => ValueProxy a b -> f
        -- |String representation of a base unit.
        showunit :: ValueProxy a b -> String

-- |Shorthand to create a composed unit containing just one base unit.
type U a = '[ '(a, P 1) ]

-- |Convertible' is a class that models the fact that the composed unit 'b' has dimension 'a'.
class Convertible' (a :: [(*, Number)]) (b :: [(*, Number)]) where
        -- |The multiplication factor to convert this unit between other units in the same dimension.
        -- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
        factor' :: (Fractional f) => ValueProxy' a b -> f
        -- |String representation of a unit.
        showunit' :: ValueProxy' a b -> String

instance (MapNull a, MapNull b) => Convertible' a b where
        {-# INLINE factor' #-}
        factor' _ = 1
        {-# INLINE showunit' #-}
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
                      in rec
                          ++ (if (not $ null rec) && (power /= 0) then "⋅" else "")
                          ++ (if power /= 0
                              then (showunit (undefined :: ValueProxy a'' unit))
                                    ++ if power /= 1
                                       then map toSuperScript $ show power
                                       else ""
                              else "")

toSuperScript :: Char -> Char
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
coerce :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c) => Value a b f -> Value c d f -> Value c d f
{-# INLINE[1] coerce #-}
coerce u into = mkVal (factor' (proxy' u) * val u / factor' (proxy' into))

{-# RULES
"coerce/id" [10] forall (x :: Value a b f) (u :: Value a b f) . coerce u x = u
"coerce/twice" [10] forall (x :: Value a b f) (y :: Value a d f) (u :: Value a i f) . coerce (coerce u y) x = coerce u x
"coerce/twice2" [10] forall (x :: Value a b f) (y :: Value a d f) (u :: Value a i f) . coerce u (coerce y x) = coerce u x
  #-}

infixl 5 `as`

-- |Shorthand for 'coerce'.
as :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c) => Value a b f -> Value c d f -> Value c d f
{-# INLINE[20] as #-}
as = coerce

-- |Shorthand for 'flip' 'coerce'
to :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c) => Value c d f -> Value a b f -> Value c d f
{-# INLINE[20] to #-}
to = flip coerce

infixl 7 |*|, |/|
infixl 6 |+|, |-|

-- |Multiply two values, constructing a value with as dimension the product of the dimensions,
-- and as unit the multplication of the units.
(|*|) :: (Fractional f, Convertible' a b, Convertible' c d) => Value a b f -> Value c d f -> Value (MapMerge a c) (MapMerge b d) f
{-# INLINE (|*|) #-}
a |*| b = mkVal (val a * val b)

-- |Divide two values, constructing a value with as dimension the division of the dimension of the lhs by the dimension of the rhs,
-- and the same for the units.
(|/|), per :: (Fractional f, Convertible' a b, Convertible' c d) => Value a b f -> Value c d f -> Value (MapMerge a (MapNeg c)) (MapMerge b (MapNeg d)) f
{-# INLINE (|/|) #-}
a |/| b = mkVal (val a / val b)
{-# INLINE per #-}
per = (|/|)

-- |Add two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(|+|) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a) => Value a b f -> Value c d f -> Value a b f
{-# INLINE (|+|) #-}
a |+| b = fmap (+ val (coerce b a)) a

-- |Subtract two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(|-|) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a) => Value a b f -> Value c d f -> Value a b f
{-# INLINE (|-|) #-}
a |-| b = fmap (\x -> x - val (coerce b a)) a

infixl 9 *|, |*, |/, /|
infixl 8 +|, |+, -|, |-

-- |Multiply a scalar by a unit.
(*|) :: (Convertible' a b, Fractional f) => f -> Value a b f -> Value a b f
{-# INLINE (*|) #-}
d *| u = fmap (d*) u

-- |Multiply a unit by a scalar.
(|*) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
{-# INLINE (|*) #-}
u |* d = fmap (*d) u

-- |Divide a scalar by a unit.
(/|) :: (Convertible' a b, Fractional f) => f -> Value a b f -> Value (MapNeg a) (MapNeg b) f
{-# INLINE (/|) #-}
d /| u = mkVal (d / val u)

-- |Divide a unit by a scalar.
(|/) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
{-# INLINE (|/) #-}
u |/ d = fmap (/d) u

-- |Add a unit to a scalar.
(+|) :: (Convertible' a b, Fractional f) => f -> Value a b f -> Value a b f
{-# INLINE (+|) #-}
d +| u = fmap (d+) u

-- |Add a scalar to a unit.
(|+) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
{-# INLINE (|+) #-}
u |+ d = fmap (+d) u

-- |Subtract a unit from a scalar.
(-|) :: (Convertible' a b, Fractional f) => f -> Value a b f -> Value a b f
{-# INLINE (-|) #-}
d -| u = fmap (d-) u

-- |Subtract a scalar from a unit.
(|-) :: (Convertible' a b, Fractional f) => Value a b f -> f -> Value a b f
{-# INLINE (|-) #-}
u |- d = fmap (\x -> x - d) u

-- |Create a new value with given scalar as value.
mkVal :: f -> Value a b f
{-# INLINE mkVal #-}
mkVal = Value

-- |Obtain the value of a value wrapped in a type.
val :: Value a b f -> f
{-# INLINE val #-}
val (Value f) = f

deriving instance (Enum f) => Enum (Value a b f)

deriving instance (Eq f) => Eq (Value a b f)

instance Functor (Value a b) where
    {-# INLINE fmap #-}
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
    foldMap f = f . val

instance Traversable (Value a b) where
    traverse f x = mkVal <$> (f $ val x)

deriving instance VG.Vector VU.Vector f => VG.Vector VU.Vector (Value a b f) 
deriving instance VGM.MVector VU.MVector f => VGM.MVector VU.MVector (Value a b f) 
deriving instance VU.Unbox f => VU.Unbox (Value a b f) 

    

-- |A wrapped value with scalar value 1.
one :: (Fractional f, Convertible' a b) => Value a b f
{-# INLINE one #-}
one = mkVal 1

-- |Calculate the square of a value. Identical to pow2, reads better on units:
--
-- >>> 100 *| square meter `as` square yard
-- 119.59900463010803 yd⋅yd⋅#
square :: (Fractional f, Convertible' c d) => Value c d f -> Value (MapMerge c c) (MapMerge d d) f
{-# INLINE square #-}
square x = x |*| x

-- |Calculate the third power of a value. Identical to pow3, reads better on units:
--
-- >>> 1 *| cubic inch `as` mili liter
-- 16.387063999999995 mL
cubic :: (Fractional f, Convertible' t t1, Convertible' c d, MapMerge d d ~ t1, MapMerge c c ~ t) => Value c d f -> Value (MapMerge (MapMerge c c) c) (MapMerge (MapMerge d d) d) f
{-# INLINE cubic #-}
cubic x = x |*| x |*| x

wrapB :: (Convertible' a b, Convertible' c d, MapEq c a) => (Rational -> Rational -> Bool) -> Value a b Rational -> Value c d Rational -> Bool
{-# INLINE wrapB #-}
wrapB op a b = op (val a) (val $ coerce b a)

infixl 4 |==|, |<|, |>|, |<=|, |>=|

(|==|), (|<|), (|>|), (|<=|), (|>=|) :: (Convertible' a b, Convertible' c d, MapEq c a) => Value a b Rational -> Value c d Rational -> Bool
-- |'==' for values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
{-# INLINE (|==|) #-}
(|==|) = wrapB (==)
-- |'<' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
{-# INLINE (|<|) #-}
(|<|) = wrapB (<)
-- |'<=' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
{-# INLINE (|<=|) #-}
(|<=|) = wrapB (<=)
-- |'>' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
{-# INLINE (|>|) #-}
(|>|) = wrapB (>)
-- |'>=' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
{-# INLINE (|>=|) #-}
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
