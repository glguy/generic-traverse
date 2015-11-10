{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

-- | This module demonstrates how to generate a list of the inhabitants
-- of a type using GHC Generics. This implementation diagonalizes
-- the list to help deal with recursive types.
--
-- The key insight here is that by reassociating all '>>=' to the right
-- using 'Codensity' that all of the generics constructors are collected
-- together. This makes it possible for GHC to optimize away the generic
-- representation.
module Boggle.Enum
  ( Enumerate(..)
    -- * Generically derived instances
  , GEnumerate(..)
    -- * Example
  , demos
  ) where

import GHC.Generics
import Data.Void                        (Void)
import Control.Applicative              (empty)
import Control.Monad.Codensity          (Codensity(..), lowerCodensity)
import Control.Monad.Logic.Class        ((>>-), interleave)

-- | This class provides a list of all the elements of a type.
class Enumerate a where
  enumerate :: [a]

  default enumerate :: (Generic a, GEnumerate (Rep a)) => [a]
  enumerate = lowerCodensity (to <$> genumerate)


class GEnumerate g where
  genumerate :: Codensity [] (g a)

instance GEnumerate f => GEnumerate (M1 i c f) where
  genumerate = M1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :+: g) where
  genumerate = L1 <$> genumerate ||| R1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :*: g) where
  genumerate = (:*:) <$> genumerate <*> genumerate

instance Enumerate b => GEnumerate (K1 i b) where
  genumerate = K1 <$> Codensity (enumerate >>-)

instance GEnumerate U1 where
  genumerate = pure U1

instance GEnumerate V1 where
  genumerate = empty

(|||) :: Codensity [] a -> Codensity [] a -> Codensity [] a
Codensity m ||| Codensity n = Codensity (\k -> interleave (m k) (n k))

infixl 3 |||

------------------------------------------------------------------------

data Demo = Z Bool | S Demo
  deriving (Show, Generic)

instance Enumerate Demo

instance Enumerate ()
instance Enumerate Void
instance Enumerate Bool
instance (Enumerate a, Enumerate b) => Enumerate (a,b)
instance (Enumerate a, Enumerate b, Enumerate c) => Enumerate (a,b,c)
instance (Enumerate a, Enumerate b) => Enumerate (Either a b)
instance Enumerate a => Enumerate [a]

demos :: [Demo]
demos = enumerate
