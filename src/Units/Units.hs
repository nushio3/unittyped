{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.Units where

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..))

-- First, type level naturals, and using those, type level integers

data Nat = One | Suc Nat

data Number = Zero | Neg Nat | Pos Nat

type Two = Pos (Suc One)
type Three = Pos (Suc (Suc One))

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


one :: (Fractional f, Convertable a b) => Value f a b
one = Value { val = 1, unit = constructor }

class Convertable a b | b -> a where
	factor :: (Fractional f) => Value f a b -> f
	constructor :: b

instance (Convertable a b, Convertable c d, UnitMerge a c u) => Convertable u (Mul b d) where
	factor :: (Fractional f) => Value f u (Mul b d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (Prelude.*) (factor left) (factor right)
	constructor :: Mul b d
	constructor = Mul constructor constructor

instance (Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Convertable u (Div b d) where
	factor :: (Fractional f) => Value f u (Div b d) -> f
	factor u = let left :: (Fractional f) => Value f a b
	               left = one
	               right ::(Fractional f) => Value f c d
	               right = one
	           in (Prelude./) (factor left) (factor right)
	constructor :: Div b d
	constructor = Div constructor constructor

---- We can coerce something of a specific dimension into any other unit in the same dimension

coerce :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d
coerce u = let result = Value {val = (Prelude./) ((Prelude.*) (factor u) (val u)) (factor result), unit = constructor} in result

coerceTo :: (Convertable a b, Convertable c d, Fractional f, UnitEq a c True) => Value f a b -> Value f c d -> Value f c d
coerceTo u _ = coerce u

data Mul b d = Mul {
	mul_l :: (Convertable a b) => b,
	mul_r :: (Convertable c d) => d
}

data Div b d = Div {
	div_l :: (Convertable a b) => b,
	div_r :: (Convertable c d) => d
}

data Value a (b :: UnitMap) c = Value {
	val :: a,
	unit :: (Convertable b c) => c
}

instance (Convertable a b, Convertable c d, Show b, Show d) => Show (Mul b d) where
	show m = (show $ mul_l m) ++ "*" ++ (show $ mul_r m)

instance (Convertable a b, Convertable c d, Show b, Show d) => Show (Div b d) where
	show m = (show $ div_l m) ++ "/(" ++ (show $ div_r m) ++ ")"

instance (Convertable b c, Show a, Show c) => Show (Value a b c) where
	show u = (show $ val u) ++ " " ++ (show $ unit u)

-- We currently have 3 operators on values-with-units: division, multiplication and addition

(*) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c u) => Value f a b -> Value f c d -> Value f u (Mul b d)
a * b = Value { val = (Prelude.*) (val a) (val b), unit = constructor }

(/) :: (Fractional f, Convertable a b, Convertable c d, UnitMerge a c' u, UnitNeg c c') => Value f a b -> Value f c d -> Value f u (Div b d)
a / b = Value { val = (Prelude./) (val a) (val b), unit = constructor }


(+) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a + b = Value { val = f a (coerce b), unit = constructor }
	where
		f :: (Fractional f) => Value f a b -> Value f a b -> f
		f a b = (Prelude.+) (val a) (val b)

(-) :: (Fractional f, Convertable a b, Convertable c d, UnitEq c a True) => Value f a b -> Value f c d -> Value f a b
a - b = Value { val = f a (coerce b), unit = constructor }
	where
		f :: (Fractional a) => Value a b c -> Value a b c -> a
		f a b = (Prelude.-) (val a) (val b)


infixl 6 +, -
infixl 7 *, /

mkVal f = Value { val = f, unit = constructor }