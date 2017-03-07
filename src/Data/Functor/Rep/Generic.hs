{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Data.Functor.Rep.Generic
  ( -- * Generic interface
    genericTabulate, genericIndex
  -- * Implementation class
  , GRepresentable(..)
  -- * Example use
  , Ix3(..), V3(..), v3Tabulate, v3Index
  ) where

import GHC.Generics

-- | Generic implementations of 'tabulate' from the @adjunctions@ package.
-- This implementation uses an enumeration type with the same number of
-- constructors as the indexed type as fields.
genericTabulate :: (Generic1 f, Generic ix, GRepresentable (Rep1 f) (Rep ix)) => (ix -> a) -> f a
genericTabulate = \f -> to1 (gtabulate (f . to))

-- | Generic implementations of 'index' from the @adjunctions@ package.
-- This implementation uses an enumeration type with the same number of
-- constructors as the indexed type as fields.
genericIndex :: (Generic1 f, Generic ix, GRepresentable (Rep1 f) (Rep ix)) => f a -> ix -> a
genericIndex = \x ix -> gindex (from1 x) (from ix)


-- | This class combines the 'Rep1' of the type to be indexed with the 'Rep'
-- of the index type to construct 'tabulate' and 'index' functions from
-- the @adjunctions@ package.
class GRepresentable f ix where
  gtabulate :: (ix p -> a) -> f a
  gindex    :: f a -> ix p -> a

-- | Ignore datatype metadata
instance GRepresentable f ix => GRepresentable (D1 d f) (D1 e ix) where
  gtabulate f = M1 (gtabulate (f . M1))
  gindex (M1 x) (M1 ix) = gindex x ix

-- | Type to be indexed must have one constructor
instance GRepresentable f ix => GRepresentable (C1 c f) ix where
  gtabulate f = M1 (gtabulate f)
  gindex (M1 x) = gindex x

-- | Match field branching with constructor branching
instance (GRepresentable f1 ix1, GRepresentable f2 ix2) =>
          GRepresentable (f1 :*: f2) (ix1 :+: ix2) where
  gtabulate f = gtabulate (f . L1) :*: gtabulate (f . R1)
  gindex (x :*: y) (L1 ix) = gindex x ix
  gindex (x :*: y) (R1 ix) = gindex y ix

-- | Match parameter field with a single index constructor
instance GRepresentable (S1 s Par1) (C1 c U1) where
  gtabulate f = M1 (Par1 (f (M1 U1)))
  gindex (M1 (Par1 x)) _ = x



-- | Type with three constructors
data Ix3 = Ix1 | Ix2 | Ix3 deriving Generic

-- | Type with three fields
data V3 a = V3 a a a       deriving Generic1

v3Tabulate :: (Ix3 -> a) -> V3 a
v3Tabulate = genericTabulate

v3Index :: V3 a -> Ix3 -> a
v3Index = genericIndex
