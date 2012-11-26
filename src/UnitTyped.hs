-- |Module defining values with dimensions and units, and mathematical operations on those.
module UnitTyped (
	Convertable(..),
	Value(..), Mul, Div,

	NoDimension, Count,

	UnitMap(..), Nat(..), Number(..),
	UnitMerge, UnitEq, UnitNeg,
	POne, PTwo, PThree, PFour, PFive, PSix,
	NOne, NTwo, NThree, NFour,
	Pow,

	coerce, as, one, mkVal, val, (.*.), (./.), (.+.), (.-.), (.$.), (~.),
	(.==.), (.<=.), (.<.), (.>=.), (.>.),

	square, cubic,

	pown3, pown2, pown1, pow0, pow1, pow2, pow3, pow4, pow5, pow6

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

-- |This is meant to be use as a datatype promoted to a kind.
-- It represents a map of dimensions to type level integers.
-- If two maps are equal, then the dimension they represent is the same.
data UnitMap where
	UnitNil :: UnitMap
	UnitCons :: a -> Number -> UnitMap -> UnitMap

class UnitAppend' q unit (value :: Number) (map :: UnitMap) (rest :: UnitMap) | q unit value map -> rest where

instance UnitAppend' q unit value UnitNil (UnitCons unit value UnitNil)
instance (Add value value' sum) =>
		 UnitAppend' () unit value (UnitCons unit value' rest) (UnitCons unit sum rest)
instance (UnitAppend' q unit value rest rest', value'' ~ value', unit'' ~ unit') =>
		 UnitAppend' q unit value (UnitCons unit' value' rest) (UnitCons unit'' value'' rest')

class UnitAppend unit (value :: Number) (map :: UnitMap) (rest :: UnitMap) | unit value map -> rest where

instance UnitAppend' () unit value map rest => UnitAppend unit value map rest where

-- |States that merging the first map with the second map produces the third argument.
-- Merging happens by summing the two values for the same key.
-- Typically, dimensions are merged when multiplicing two values.
class UnitMerge (map1 :: UnitMap) (map2 :: UnitMap) (rest :: UnitMap) | map1 map2 -> rest where

instance UnitMerge UnitNil map2 map2
instance (UnitMerge rest map2 rest2, UnitAppend unit value rest2 rec) => UnitMerge (UnitCons unit value rest) map2 rec

-- |States that 'rest' is the same dimension as 'map1', but all integers inverted.
-- Used for division.
class UnitNeg (map1 :: UnitMap) (rest :: UnitMap) | map1 -> rest where

instance UnitNeg UnitNil UnitNil
instance (Negate value value', UnitNeg rest rest') => UnitNeg (UnitCons unit value rest) (UnitCons unit value' rest')

class UnitNull (map :: UnitMap) (b :: Bool) | map -> b where

instance UnitNull UnitNil True
instance (UnitNull rest b', IsZero value b, And b b' result) => UnitNull (UnitCons unit value rest) result

-- |'b' is equal to 'True' if and only if 'map1' and 'map2' represent the same dimension.
class UnitEq (map1 :: UnitMap) (map2 :: UnitMap) (b :: Bool) | map1 map2 -> b where

instance (UnitNeg map2 map2', UnitMerge map1 map2' sum, UnitNull sum b) => UnitEq map1 map2 b

-- |Convertable is a class that models the fact that the unit 'b' has dimension 'a' (of kind 'UnitMap').
class Convertable (a :: UnitMap) b | b -> a where
	-- |The multiplication factor to convert this unit between other units in the same dimension.
	-- Only the ratio matters, which one is '1' is not important, as long as all are consistent.
	factor :: (Fractional f) => Value f a b -> f
	-- |String representation of a unit. The boolean determines wether to use brackets (only important for the denomiator).
	-- The value should not be important for the output, its only here because it needs to be a class method.
	showunit :: (Fractional f) => Bool -> Value f a b -> String

instance (Convertable a b, Convertable c d, UnitMerge a c u) => Convertable u (Mul b d) where
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (factor left) * (factor right)
	showunit b u = let left :: (Fractional f) => Value f a b
	                   left = one
	                   right ::(Fractional f) => Value f c d
	                   right = one
	                   rest = (showunit False left) ++ "⋅" ++ (showunit False right)
	               in if b then "(" ++ rest ++ ")" else rest

instance (Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Convertable u (Div b d) where
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (factor left) / (factor right)
	showunit b u = let left :: (Fractional f) => Value f a b
	                   left = one
	                   right ::(Fractional f) => Value f c d
	                   right = one
	                   rest = (showunit False left) ++ "/" ++ (showunit True right)
	                in if b then "(" ++ rest ++ ")" else rest

-- |coerce something of a specific dimension into any other unit in the same dimension.
-- The second argument is only used for its type, but it allows nice syntax like:
--
-- >>> coerce (120 meter / second) (kilo meter / hour)
-- 432.0 km/h
coerce :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
coerce u _ = let result = mkVal (factor u * val u / factor result) in result

infixl 5 ~.

-- |Shorthand for 'coerce'.
(~.) :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
(~.) = coerce

infixl 5 `as`

-- |Shorthand for 'coerce'.
as :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
as = coerce

-- |A unit representing the multplication of the units b and d.
data Mul b d

-- |A unit representing the division of the units b by d.
data Div b d

-- |A value tagged with its dimension b and unit c.
data Value a (b :: UnitMap) c = Value a

instance (Convertable a b) => Show b where
	show _ = showunit False one
		where
			one :: (Fractional f) => Value f a b
			one = one

instance (Fractional f, Show f, Convertable a b, Show b) => Show (Value f a b) where
	show u = show (val u) ++ " " ++ showunit False u

-- |Multiply two values, constructing a value with as dimension the product of the dimensions,
-- and as unit the multplication of the units.
(.*.) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c u) => Value f a b -> Value f c d -> Value f u (Mul b d)
a .*. b = mkVal (val a * val b)

-- |Divide two values, constructing a value with as dimension the division of the dimension of the lhs by the dimension of the rhs,
-- and the same for the units.
(./.), per :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Value f a b -> Value f c d -> Value f u (Div b d)
a ./. b = mkVal (val a / val b)
per = (./.)

-- |Add two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(.+.) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a .+. b = mkVal (val a + val (coerce b a))

-- |Subtract two values with matching dimensions. Units are automatically resolved. The result will have the same unit as the lhs.
(.-.) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a .-. b = mkVal (val a - val (coerce b a))

-- |Multiply a unit by a scalar.
(.$.) :: (Convertable a b, Fractional f) => f -> Value f a b -> Value f a b
d .$. u = mkVal (d * val u)

-- |Create a new value with given scalar as value.
mkVal :: (Fractional f) => f -> Value f a b
mkVal = Value

-- |Obtain the value of a value wrapped in a type.
val :: (Fractional f) => Value f a b -> f
val (Value f) = f

-- |A wrapped value with scalar value 1.
one :: (Fractional f, Convertable a b) => Value f a b
one = mkVal 1

-- |Calculate the square of a value. Identical to pow2, reads better on units:
-- 
-- >>> 100 . square meter `as` square yard
-- 119.59900463010803 yd⋅yd⋅#
square :: (Fractional f, Convertable a b, Pow a b PTwo c d) => Value f a b -> Value f c d
square = pow (Proxy :: Proxy PTwo)

-- |Calculate the third power of a value. Identical to pow3, reads better on units:
-- 
-- >>> 1 . cubic inch `as` mili liter
-- 16.387063999999995 mL
cubic :: (Fractional f, Convertable a b, Pow a b PThree c d) => Value f a b -> Value f c d
cubic = pow (Proxy :: Proxy PThree)

-- |Calculate @x^(-3)@.
pown3 :: (Fractional f, Convertable a b, Pow a b NThree c d) => Value f a b -> Value f c d
pown3 = pow (Proxy :: Proxy NThree)

-- |Calculate @x^(-2)@.
pown2 :: (Fractional f, Convertable a b, Pow a b NTwo c d) => Value f a b -> Value f c d
pown2 = pow (Proxy :: Proxy NTwo)

-- |Calculate @x^(-1)@.
pown1 :: (Fractional f, Convertable a b, Pow a b NOne c d) => Value f a b -> Value f c d
pown1 = pow (Proxy :: Proxy NOne)

-- |Calculate @x^0@. Yes, this is always @one :: Value f NoDimension Count@.
pow0 :: (Fractional f, Convertable a b, Pow a b Zero c d) => Value f a b -> Value f c d
pow0 = pow (Proxy :: Proxy Zero)

-- |Calculate @x^1@.
pow1 :: (Fractional f, Convertable a b, Pow a b POne c d) => Value f a b -> Value f c d
pow1 = pow (Proxy :: Proxy POne)

-- |Calculate @x^2@.
pow2 :: (Fractional f, Convertable a b, Pow a b PTwo c d) => Value f a b -> Value f c d
pow2 = pow (Proxy :: Proxy PTwo)

-- |Calculate @x^3@.
pow3 :: (Fractional f, Convertable a b, Pow a b PThree c d) => Value f a b -> Value f c d
pow3 = pow (Proxy :: Proxy PThree)

-- |Calculate @x^4@.
pow4 :: (Fractional f, Convertable a b, Pow a b PFour c d) => Value f a b -> Value f c d
pow4 = pow (Proxy :: Proxy PFour)

-- |Calculate @x^5@.
pow5 :: (Fractional f, Convertable a b, Pow a b PFive c d) => Value f a b -> Value f c d
pow5 = pow (Proxy :: Proxy PFive)

-- |Calculate @x^6@.
pow6 :: (Fractional f, Convertable a b, Pow a b PSix c d) => Value f a b -> Value f c d
pow6 = pow (Proxy :: Proxy PSix)

wrapB :: (Convertable a b, Convertable c d, UnitEq c a True) => (Rational -> Rational -> Bool) -> Value Rational a b -> Value Rational c d -> Bool
wrapB op a b = op (val a) (val $ coerce b a)

(.==.), (.<.), (.>.), (.<=.), (.>=.) :: (Convertable a b, Convertable c d, UnitEq c a True) => Value Rational a b -> Value Rational c d -> Bool
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

-- |This is for dimensionless values.
type NoDimension = UnitNil

--

-- |One thing.
data Count

instance Convertable NoDimension Count where
	factor _ = 1
	showunit _ _ = "#"

--

data Proxy (i :: Number) = Proxy

class (Convertable a b, Convertable c d) => Pow' q a b (i :: Number) c d | q a b i -> c d where
	_pow :: (Fractional f) => q -> Proxy i -> Value f a b -> Value f c d

instance (Convertable a b) => Pow' () a b Zero UnitNil Count where
	_pow _ _ _ = one

instance (Convertable a b, Convertable a'' b'', Pow' q a b i' a' b', UnitNeg a' a'', Negate (Neg i) i', b'' ~ (Div Count b')) => Pow' q a b (Neg i) a'' b'' where
	_pow q p x = one ./. (_pow q (Proxy :: Proxy i') x)

instance (Convertable a b, Convertable a'' b'', Pow' q a b i' a' b', Pre' (Pos i) i', UnitMerge a a' a'', b'' ~ (Mul b b')) => Pow' q a b (Pos i) a'' b'' where
	_pow q p x = x .*. (_pow q (Proxy :: Proxy i') x)

-- |'^' is not definable on 'Value's in general, as the result depends on the exponent.
-- However, we can use this class to raise a unit to a type level 'Number'.
class (Convertable a b, Convertable c d) => Pow a b (i :: Number) c d | a b -> c, a b -> d where
	pow :: (Fractional f) => Proxy i -> Value f a b -> Value f c d

instance (Pow' () a b i c d) => Pow a b (i :: Number) c d where
	pow = _pow ()
