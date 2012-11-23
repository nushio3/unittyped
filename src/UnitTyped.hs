module UnitTyped where

-- First, type level naturals, and using those, type level integers

data Nat = One | Suc Nat

data Number = Zero | Neg Nat | Pos Nat

type POne = Pos One
type PTwo = Pos (Suc One)
type PThree = Pos (Suc (Suc One))
type PFour = Pos (Suc (Suc (Suc One)))

type NOne = Neg One
type NTwo = Neg (Suc One)
type NThree = Neg (Suc (Suc One))
type MFour = Neg (Suc (Suc (Suc One)))

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

-- Now we create a type level list of [(Unit, Integer)], that act like maps
-- With the option to append values, merge two lists and negate a list

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

class UnitMerge (map1 :: UnitMap) (map2 :: UnitMap) (rest :: UnitMap) | map1 map2 -> rest where

instance UnitMerge UnitNil map2 map2
instance (UnitMerge rest map2 rest2, UnitAppend unit value rest2 rec) => UnitMerge (UnitCons unit value rest) map2 rec

class UnitNeg (map1 :: UnitMap) (rest :: UnitMap) | map1 -> rest where

instance UnitNeg UnitNil UnitNil
instance (Negate value value', UnitNeg rest rest') => UnitNeg (UnitCons unit value rest) (UnitCons unit value' rest')

class UnitNull (map :: UnitMap) (b :: Bool) | map -> b where

instance UnitNull UnitNil True
instance (UnitNull rest b', IsZero value b, And b b' result) => UnitNull (UnitCons unit value rest) result

class UnitEq (map1 :: UnitMap) (map2 :: UnitMap) (b :: Bool) | map1 map2 -> b where

instance (UnitNeg map2 map2', UnitMerge map1 map2' sum, UnitNull sum b) => UnitEq map1 map2 b

----
---- Convertable a b means "b is a unit for dimension a"
----

class Convertable a b | b -> a where
	factor :: (Fractional f) => Value f a b -> f
	showunit :: (Fractional f) => Bool -> Value f a b -> String

instance (Convertable a b, Convertable c d, UnitMerge a c u) => Convertable u (Mul b d) where
	factor :: (Fractional f) => Value f u (Mul b d) -> f
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
	factor :: (Fractional f) => Value f u (Div b d) -> f
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

---- We can coerce something of a specific dimension into any other unit in the same dimension
---- The second argument is only used for its type, but it allows nice syntace like coerce (120 meter / second) (kilo meter / hour)

coerce :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
coerce u _ = let result = mkVal (factor u * val u / factor result) in result

infixl 5 ~.

(~.) :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
(~.) = coerce

infixl 5 `as`

as :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
as = coerce

data Mul b d

data Div b d

data Value a (b :: UnitMap) c = Value a

instance (Convertable a b) => Show b where
	show _ = showunit False one
		where
			one :: (Fractional f) => Value f a b
			one = one

instance (Fractional f, Show f, Convertable a b, Show b) => Show (Value f a b) where
	show u = show (val u) ++ " " ++ showunit False u

-- We currently have 5 operators on values-with-units: division, multiplication, addition, substraction and lifting a rational into a given unit

(.*.) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c u) => Value f a b -> Value f c d -> Value f u (Mul b d)
a .*. b = mkVal (val a * val b)

(./.), per :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Value f a b -> Value f c d -> Value f u (Div b d)
a ./. b = mkVal (val a / val b)
per = (./.)

(.+.) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a .+. b = mkVal (val a + val (coerce b a))

(.-.) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a .-. b = mkVal (val a - val (coerce b a))

(.$.) :: (Convertable a b, Fractional f) => f -> Value f a b -> Value f a b
d .$. u = mkVal (d * val u)

mkVal :: (Fractional f) => f -> Value f a b
mkVal = Value

val :: (Fractional f) => Value f a b -> f
val (Value f) = f

one :: (Fractional f, Convertable a b) => Value f a b
one = mkVal 1

square :: (Fractional f, Convertable c d, UnitMerge c c u) => Value f c d -> Value f u (Mul d d)
square x = x .*. x

cubic :: (Fractional f, Convertable c d, UnitMerge c c a, UnitMerge a c u) => Value f c d -> Value f u (Mul (Mul d d) d)
cubic x = x .*. x .*. x

pown3 :: (Fractional f, UnitNeg c1 c', UnitMerge a1 c' u, UnitMerge c c a, UnitMerge a c c1, Convertable a1 b, Convertable c d) => Value f c d -> Value f u (Div b (Mul (Mul d d) d))
pown3 x = one ./. (x .*. x .*. x)

pown2 :: (Fractional f, Convertable c d, UnitMerge c c c1, UnitNeg c1 c2) => Value f c d -> Value f c2 (Div Count (Mul d d))
pown2 x = one ./. (x .*. x)

pown1 :: (Fractional f, Convertable c d, UnitNeg c c') => Value f c d -> Value f c' (Div Count d)
pown1 x = one ./. x

pow0 :: (Fractional f, Convertable c d) => Value f c d -> Value f UnitNil Count
pow0 _ = one

pow1 :: (Fractional f, Convertable c d) => Value f c d -> Value f c d
pow1 = id

pow2 :: (Fractional f, Convertable c d, UnitMerge c c u) => Value f c d -> Value f u (Mul d d)
pow2 = square

pow3 :: (Fractional f, Convertable c d, UnitMerge c c a, UnitMerge a c u) => Value f c d -> Value f u (Mul (Mul d d) d)
pow3 = cubic

pow4 :: (Fractional f, UnitMerge c c a, UnitMerge a c a1, UnitMerge a1 c u, Convertable c d) => Value f c d -> Value f u (Mul (Mul (Mul d d) d) d)
pow4 x = x .*. x .*. x .*. x

pow5 :: (Fractional f, UnitMerge a c b, UnitMerge b c u, UnitMerge c c e, UnitMerge e c a, Convertable c d) => Value f c d -> Value f u (Mul (Mul (Mul (Mul d d) d) d) d)
pow5 x = x .*. x .*. x .*. x .*. x

pow6 :: (Fractional f, UnitMerge a c a3, UnitMerge a3 c u, UnitMerge a1 c a, UnitMerge c c a2, UnitMerge a2 c a1, Convertable c d) => Value f c d -> Value f u (Mul (Mul (Mul (Mul (Mul d d) d) d) d) d)
pow6 x = x .*. x .*. x .*. x .*. x .*. x

wrapB :: (Convertable a b, Convertable c d, UnitEq c a True) => (Rational -> Rational -> Bool) -> Value Rational a b -> Value Rational c d -> Bool
wrapB op a b = op (val a) (val $ coerce b a)

(.==.), (.<.), (.>.), (.<=.), (.>=.) :: (Convertable a b, Convertable c d, UnitEq c a True) => Value Rational a b -> Value Rational c d -> Bool
(.==.) = wrapB (==)
(.<.) = wrapB (<)
(.<=.) = wrapB (<=)
(.>.) = wrapB (>)
(.>=.) = wrapB (>=)

----
-- Counting. These are dimension-less values.
----


type CountUnit = UnitNil

--

data Count

instance Convertable CountUnit Count where
	factor _ = 1
	showunit _ _ = "#"