{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Data.Functor.Rep.Generic
  ( -- * Generic interface
    genericTabulate, genericIndex
  -- * Implementation class
  , GRepresentable(..)
  ) where

import GHC.Generics

genericTabulate :: (Generic1 f, Generic ix, GRepresentable (Rep1 f) (Rep ix)) => (ix -> a) -> f a
genericTabulate = \f -> to1 (gtabulate (f . to))

genericIndex :: (Generic1 f, Generic ix, GRepresentable (Rep1 f) (Rep ix)) => f a -> ix -> a
genericIndex = \x ix -> gindex (from1 x) (from ix)


class GRepresentable f ix where
  gtabulate :: (ix p -> a) -> f a
  gindex    :: f a -> ix p -> a

instance GRepresentable f ix => GRepresentable (D1 d (C1 c f)) (D1 e ix) where
  gtabulate f = M1 (M1 (gtabulate (f . M1)))
  gindex (M1 (M1 x)) (M1 ix) = gindex x ix

instance (GRepresentable f1 ix1, GRepresentable f2 ix2) =>
          GRepresentable (f1 :*: f2) (ix1 :+: ix2) where
  gtabulate f = gtabulate (f . L1) :*: gtabulate (f . R1)
  gindex (x :*: y) (L1 ix) = gindex x ix
  gindex (x :*: y) (R1 ix) = gindex y ix

instance GRepresentable (S1 s Par1) (C1 c U1) where
  gtabulate f = M1 (Par1 (f (M1 U1)))
  gindex (M1 (Par1 x)) _ = x
