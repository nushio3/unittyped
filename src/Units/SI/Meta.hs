{-# LANGUAGE FlexibleInstances, RankNTypes, MultiParamTypeClasses, ConstraintKinds, DataKinds, UndecidableInstances, FunctionalDependencies, KindSignatures, TypeFamilies, InstanceSigs, ScopedTypeVariables, FlexibleContexts, TypeOperators, OverlappingInstances, ImpredicativeTypes, GADTs #-}

module Units.SI.Meta where

import Units.SI
import Units.Units

import qualified Prelude
import Prelude (Show(..), Fractional, ($), (.), (++), Double, const, Bool(..), otherwise, undefined, String(..))

----
-- Meta-units
----

class MetaUnit (m :: * -> *) where
	metaconstructor :: a -> m a
	metafactor :: (Fractional f) => Value f a (m b) -> f
	metashow :: (Fractional f, Show a) => m a -> String

instance (MetaUnit m, Convertable a b) => Convertable a (m b) where
	factor :: (Fractional f) => Value f a (m b) -> f
	factor _ = let sub :: (Fractional f) => Value f a b
	               sub = one
	               self :: (Fractional f) => Value f a (m b)
	               self = one
	           in (Prelude.*) (metafactor self) (factor sub)
	showunit = metashow

--

data Kilo a = Kilo a

instance MetaUnit Kilo where
	metaconstructor = Kilo
	metafactor _ = 1000
	metashow (Kilo u) = "k" ++ (show u)

data Mega a = Mega a

instance MetaUnit Mega where
	metaconstructor = Mega
	metafactor _ = 1000000
	metashow (Mega u) = "M" ++ (show u)

data Giga a = Giga a

instance MetaUnit Giga where
	metaconstructor = Giga
	metafactor _ = 1000000000
	metashow (Giga u) = "G" ++ (show u)

data Tera a = Tera a

instance MetaUnit Tera where
	metaconstructor = Tera
	metafactor _ = 1000000000000
	metashow (Tera u) = "T" ++ (show u)

data Mili a = Mili a

instance MetaUnit Mili where
	metaconstructor = Mili
	metafactor _ = 0.001
	metashow (Mili u) = "m" ++ (show u)

----
-- 2 factors
----

data Kibi a = Kibi a

instance MetaUnit Kibi where
	metaconstructor = Kibi
	metafactor _ = 1024
	metashow (Kibi u) = "Ki" ++ (show u)

data Mebi a = Mebi a

instance MetaUnit Mebi where
	metaconstructor = Mebi
	metafactor _ = (Prelude.*) 1024 1024
	metashow (Mebi u) = "Mi" ++ (show u)

data Gibi a = Gibi a

instance MetaUnit Gibi where
	metaconstructor = Gibi
	metafactor _ = (Prelude.*) 1024 ((Prelude.*) 1024 1024)
	metashow (Gibi u) = "Gi" ++ (show u)

data Tebi a = Tebi a

instance MetaUnit Tebi where
	metaconstructor = Tebi
	metafactor _ = (Prelude.*) ((Prelude.*) 1024 1024) ((Prelude.*) 1024 1024)
	metashow (Tebi u) = "Ti" ++ (show u)

--

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