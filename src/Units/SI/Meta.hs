{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.SI.Meta where

import Units.SI
import Units.Units

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..))

----
-- Meta-units
----

class (Convertable a b, Show b) => MetaUnit (m :: * -> *) a b where
	metafactor :: (Fractional f) => Value f a (m b) -> f
	metashow :: (Fractional f) => Value f a (m b) -> String

instance (MetaUnit m a b, Convertable a b) => Convertable a (m b) where
	factor :: (Fractional f) => Value f a (m b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	               self :: (Fractional f) => Value f a (m b)
	               self = one
	           in (Prelude.*) (metafactor self) (factor sub)
	showunit b v = let
			 		   u :: (Fractional f) => Value f a b
					   u = one
				   in metashow v ++ (showunit False u) 

--

data Kilo a

instance (Convertable a b) => MetaUnit Kilo a b where
	metafactor _ = 1000
	metashow _ = "k"

data Mega a

instance (Convertable a b) => MetaUnit Mega a b where
	metafactor _ = 1000000
	metashow _ = "M"

data Giga a

instance (Convertable a b) => MetaUnit Giga a b where
	metafactor _ = 1000000000
	metashow _ = "G"

data Tera a

instance (Convertable a b) => MetaUnit Tera a b where
	metafactor _ = 1000000000000
	metashow _ = "T"

data Mili a

instance (Convertable a b) => MetaUnit Mili a b where
	metafactor _ = 0.001
	metashow _ = "m"

------
---- 2 factors
------

data Kibi a

instance (Convertable a b) => MetaUnit Kibi a b where
	metafactor _ = 1024
	metashow _ = "Ki"

data Mebi a

instance (Convertable a b) => MetaUnit Mebi a b where
	metafactor _ = (Prelude.*) 1024 1024
	metashow _ = "Mi"

data Gibi a

instance (Convertable a b) => MetaUnit Gibi a b where
	metafactor _ = (Prelude.*) 1024 ((Prelude.*) 1024 1024)
	metashow _ = "Gi"

data Tebi a

instance (Convertable a b) => MetaUnit Tebi a b where
	metafactor _ = (Prelude.*) ((Prelude.*) 1024 1024) ((Prelude.*) 1024 1024)
	metashow _ = "Ti"

----

kilo :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Kilo b)
kilo _ = mkVal

mili :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Mili b)
mili _ = mkVal

mega :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Mega b)
mega _ = mkVal

giga :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Giga b)
giga _ = mkVal

tera :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Tera b)
tera _ = mkVal

--

mebi :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Mebi b)
mebi _ = mkVal

gibi :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Gibi b)
gibi _ = mkVal

tebi :: (Fractional f) => (f -> Value f a b) -> f -> Value f a (Tebi b)
tebi _ = mkVal