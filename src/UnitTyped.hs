{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeFamilies #-}
-- |Module defining values with dimensions and units, and mathematical operations on those.
module UnitTyped (
	Convertible(..), Convertible'(..),
	Value(..), ValueProxy(..), proxy',

	Count,

	Nat(..), Number(..),
	MapMerge, MapEq, MapNeg, MapTimes, MapStrip,
	POne, PTwo, PThree, PFour, PFive, PSix,
	NOne, NTwo, NThree, NFour,
	
	coerce, as, to, one, mkVal, val, mapVal, (.*.), (./.), (.+.), (.-.), (~>), (~.),
	(.==.), (.<=.), (.<.), (.>=.), (.>.),

	square, cubic,

	{- pown3, pown2, pown1, pow0, pow1, pow2, pow3, pow4, pow5, pow6 -}

) where

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
data Value f (a :: [(*, Number)]) (b :: [(*, Number)]) = Value f
data ValueProxy (a :: [(*, Number)]) b = ValueProxy
data ValueProxy' (a :: [(*, Number)]) (b :: [(*, Number)]) = ValueProxy'
data NumberProxy (a :: Number) = NumberProxy

proxy' :: (Convertible' a b) => Value f a b -> ValueProxy' a b
proxy' _ = ValueProxy'

class FromNumber (a :: Number) where
	fromNumber :: NumberProxy a -> Integer

instance FromNumber Zero where
	fromNumber _ = 0

instance FromNumber (Pos One) where
	fromNumber _ = 1

instance (FromNumber (Pos a)) => FromNumber (Pos (Suc a)) where
	fromNumber _ = 1 + (fromNumber (NumberProxy :: NumberProxy (Pos a)))

instance FromNumber (Neg One) where
	fromNumber _ = -1

instance (FromNumber (Neg a)) => FromNumber (Neg (Suc a)) where
	fromNumber _ = -1 + (fromNumber (NumberProxy :: NumberProxy (Neg a)))

class Convertible (a :: [(*, Number)]) b | b -> a where
	factor :: (Fractional f) => ValueProxy a b -> f
	showunit :: (Fractional f) => ValueProxy a b -> String

-- |Convertible is a class that models the fact that the unit 'b' has dimension 'a' (of kind '[(*, Number)]').
class Convertible' (a :: [(*, Number)]) (b :: [(*, Number)]) where
	-- |The multiplication factor to convert this unit between other units in the same dimension.
	-- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
	factor' :: (Fractional f) => ValueProxy' a b -> f
	-- |String representation of a unit. The boolean determines wether to use brackets (only important for the denomiator).
	-- The value should not be important for the output, its only here because it needs to be a class method.
	showunit' :: (Fractional f) => ValueProxy' a b -> String

--instance Convertible' '[] '[] where
--	factor' _ = 1
--	showunit' _ = ""

instance (MapNull a True) => Convertible' a '[] where
	factor' _ = 1
	showunit' _ = ""

--instance (MapNull a True) => Convertible' a '[] where
--	factor' _ = 1
--	showunit' _ = ""

instance (Convertible a b, MapEq a a' True) => Convertible' a' ('(b, POne) ': '[]) where
	factor' _ = factor (ValueProxy :: ValueProxy a b)
	showunit' _ = showunit (ValueProxy :: ValueProxy a b)

instance (FromNumber value, Convertible' rec_dimension rest, MapNeg unit_dimension neg_unit_dimension,
		  MapTimes value neg_unit_dimension times_neg_unit_dimension, MapMerge times_neg_unit_dimension dimension rec_dimension,
		  Convertible unit_dimension unit) => Convertible' dimension ('(unit, value) ': rest) where
	factor' _ = let
					rec = factor' (ValueProxy' :: ValueProxy' rec_dimension rest)
				in rec * ((factor (ValueProxy :: ValueProxy unit_dimension unit)) ^^ (fromNumber (NumberProxy :: NumberProxy value)))
	showunit' _ = let
					rec = showunit' (ValueProxy' :: ValueProxy' rec_dimension rest)
					power = fromNumber (NumberProxy :: NumberProxy value)
				  in (if null rec then "" else rec) ++ (if (not $ null rec) && (power /= 0) then "⋅" else "") ++ (if power /= 0 then (showunit (ValueProxy :: ValueProxy a'' unit)) ++ (if power /= 1 then "^" ++ show power else "") else "")


instance (Fractional f, Show f, Convertible' a b) => Show (Value f a b) where
	show u = show (val u) ++ " " ++ showunit' (proxy' u)

-- |coerce something of a specific dimension into any other unit in the same dimension.
-- The second argument is only used for its type, but it allows nice syntax like:
--
-- >>> coerce (120 meter / second) (kilo meter / hour)
-- 432.0 km/h
coerce :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c True) => Value f a b -> Value f c d -> Value f c d
coerce u _ = let result = mkVal (factor' (proxy' u) * val u / factor' (proxy' result)) in result

infixl 5 ~.
infixl 8 ~>

-- |Shorthand for 'coerce'.
(~.) :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c True) => Value f a b -> Value f c d -> Value f c d
(~.) = coerce

infixl 5 `as`

-- |Shorthand for 'coerce'.
as :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c True) => Value f a b -> Value f c d -> Value f c d
as = coerce

-- |Shorthand for 'flip' 'coerce'
to :: (Convertible' a b, Convertible' c d, Fractional f, MapEq a c True) => Value f c d -> Value f a b -> Value f c d
to = flip coerce

infixl 7 .*., ./.
infixl 6 .+., .-.

-- |Multiply two values, constructing a value with as dimension the product of the dimensions,
-- and as unit the multplication of the units.
(.*.) :: (Fractional f, Convertible' a b, Convertible' c d, MapMerge a c u, MapMerge b d s) => Value f a b -> Value f c d -> Value f u s
a .*. b = mkVal (val a * val b)

-- |Divide two values, constructing a value with as dimension the division of the dimension of the lhs by the dimension of the rhs,
-- and the same for the units.
(./.), per :: (Fractional f, Convertible' a b, Convertible' c d, MapMerge a c' u, MapNeg c c', MapNeg d d', MapMerge b d' s) => Value f a b -> Value f c d -> Value f u s
a ./. b = mkVal (val a / val b)
per = (./.)

-- |Add two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(.+.) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a True) => Value f a b -> Value f c d -> Value f a b
a .+. b = mkVal (val a + val (coerce b a))

-- |Subtract two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(.-.) :: (Fractional f, Convertible' a b, Convertible' c d, MapEq c a True) => Value f a b -> Value f c d -> Value f a b
a .-. b = mkVal (val a - val (coerce b a))

-- |Multiply a unit by a scalar.
(~>) :: (Convertible' a b, Fractional f) => f -> Value f a b -> Value f a b
d ~> u = mkVal (d * val u)

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
-- >>> 100 . square meter `as` square yard
-- 119.59900463010803 yd⋅yd⋅#
square :: (Fractional f, MapMerge c c u, MapMerge d d s, Convertible' c d) => Value f c d -> Value f u s
square x = x .*. x

-- |Calculate the third power of a value. Identical to pow3, reads better on units:
-- 
-- >>> 1 . cubic inch `as` mili liter
-- 16.387063999999995 mL
cubic   :: (Fractional f, MapMerge a c u, MapMerge b d s, MapMerge c c a, MapMerge d d b, Convertible' a b, Convertible' c d) => Value f c d -> Value f u s
cubic x = x .*. x .*. x

-- |Calculate @x^(-3)@.
pown3 :: (Fractional f, Convertible' a b, Pow a b NThree c d) => Value f a b -> Value f c d
pown3 = pow (NumberProxy :: NumberProxy NThree)

-- |Calculate @x^(-2)@.
pown2 :: (Fractional f, Convertible' a b, Pow a b NTwo c d) => Value f a b -> Value f c d
pown2 = pow (NumberProxy :: NumberProxy NTwo)

-- |Calculate @x^(-1)@.
pown1 :: (Fractional f, Convertible' a b, Pow a b NOne c d) => Value f a b -> Value f c d
pown1 = pow (NumberProxy :: NumberProxy NOne)

-- |Calculate @x^0@. Yes, this is always @one :: Value f ('[] Count@.
pow0 :: (Fractional f, Convertible' a b, Pow a b Zero c d) => Value f a b -> Value f c d
pow0 = pow (NumberProxy :: NumberProxy Zero)

-- |Calculate @x^1@.
pow1 :: (Fractional f, Convertible' a b, Pow a b POne c d) => Value f a b -> Value f c d
pow1 = pow (NumberProxy :: NumberProxy POne)

-- |Calculate @x^2@.
pow2 :: (Fractional f, Convertible' a b, Pow a b PTwo c d) => Value f a b -> Value f c d
pow2 = pow (NumberProxy :: NumberProxy PTwo)

-- |Calculate @x^3@.
pow3 :: (Fractional f, Convertible' a b, Pow a b PThree c d) => Value f a b -> Value f c d
pow3 = pow (NumberProxy :: NumberProxy PThree)

-- |Calculate @x^4@.
pow4 :: (Fractional f, Convertible' a b, Pow a b PFour c d) => Value f a b -> Value f c d
pow4 = pow (NumberProxy :: NumberProxy PFour)

-- |Calculate @x^5@.
pow5 :: (Fractional f, Convertible' a b, Pow a b PFive c d) => Value f a b -> Value f c d
pow5 = pow (NumberProxy :: NumberProxy PFive)

-- |Calculate @x^6@.
pow6 :: (Fractional f, Convertible' a b, Pow a b PSix c d) => Value f a b -> Value f c d
pow6 = pow (NumberProxy :: NumberProxy PSix)

wrapB :: (Convertible' a b, Convertible' c d, MapEq c a True) => (Rational -> Rational -> Bool) -> Value Rational a b -> Value Rational c d -> Bool
wrapB op a b = op (val a) (val $ coerce b a)

infixl 4 .==., .<., .>., .<=., .>=.

(.==.), (.<.), (.>.), (.<=.), (.>=.) :: (Convertible' a b, Convertible' c d, MapEq c a True) => Value Rational a b -> Value Rational c d -> Bool
-- |'==' for values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(.==.) = wrapB (==)
-- |'<' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(.<.) = wrapB (<)
-- |'<=' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(.<=.) = wrapB (<=)
-- |'>' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(.>.) = wrapB (>)
-- |'>=' on values. Only defined for values with rational contents. Can be used on any two values with the same dimension.
(.>=.) = wrapB (>=)

----
-- Counting. These are dimension-less values.
----

--

-- |One thing.
data Count

instance Convertible '[] Count where
	factor _ = 1
	showunit _ = "#"

--

class (Convertible' a b, Convertible' c d) => Pow' q a b (i :: Number) c d | q a b i -> c d where
	_pow :: (Fractional f) => q -> NumberProxy i -> Value f a b -> Value f c d

instance (Convertible' c d, MapNull c True, MapNull d True, Convertible' a b) => Pow' () a b Zero c d where
	_pow _ _ _ = one

instance (Convertible' a b, Convertible' a'' b'', Pow' q a b i' a' b', Pre' (Pos i) i', MapMerge a a' a'', MapMerge b b' b'') => Pow' q a b (Pos i) a'' b'' where
	_pow _ _ _ = one

-- |'^' is not definable on 'Value's in general, as the result depends on the exponent.
-- However, we can use this class to raise a unit to a type level 'Number'.
class (Convertible' a b, Convertible' c d, Pow' () a b i c d) => Pow a b (i :: Number) c d | a b -> c d where
	pow :: (Fractional f) => NumberProxy i -> Value f a b -> Value f c d

instance (Convertible' a b, Pow' () a b i c d) => Pow a b (i :: Number) c d where
	pow = _pow ()

--
