{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}

-- | This module demonstrates how to generate a list of the inhabitants
-- of a type using GHC Generics. This implementation diagonalizes
-- the list to help deal with recursive types.
--
-- The key insight here is that by reassociating all '>>=' to the right
-- using 'BindK' that all of the generics constructors are collected
-- together. This makes it possible for GHC to optimize away the generic
-- representation.
module Boggle.Enum
  ( Enumerate(..)
    -- * Generically derived instances
  , GEnumerate(..)
  , BindK(..), lowerBindK, (|||)
    -- * List helper functions
  , interleave
  , (>>-)
    -- * Example
  , Demo(..), demos
  ) where

import Control.Applicative      (Alternative(..), liftA)
import Data.Void                (Void)
import GHC.Generics

-- | This class provides a list of all the elements of a type.
class Enumerate a where
  enumerate :: [a]

  default enumerate :: (Generic a, GEnumerate (Rep a)) => [a]
  enumerate = lowerBindK (to <$> genumerate)


class GEnumerate g where
  genumerate :: BindK [] (g a)

instance GEnumerate f => GEnumerate (M1 i c f) where
  genumerate = M1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :+: g) where
  genumerate = L1 <$> genumerate ||| R1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :*: g) where
  genumerate = (:*:) <$> genumerate <*> genumerate

instance Enumerate b => GEnumerate (K1 i b) where
  genumerate = K1 <$> BindK (enumerate >>-)

instance GEnumerate U1 where
  genumerate = pure U1

instance GEnumerate V1 where
  genumerate = empty

------------------------------------------------------------------------

-- | Local implementation of @Codensity@ type from @kan-extensions@.
-- This type captures the concept of a partially applied '>>=' function.
--
-- While this type support many other instances we focus on the ones
-- needed to implement this example.
newtype BindK f a = BindK (forall b. (a -> f b) -> f b)

instance Functor (BindK f) where
  fmap = liftA

instance Applicative (BindK f) where
  pure x              = BindK $ \k  -> k x
  BindK m <*> BindK n = BindK $ \k  ->
                            m $ \ab ->
                            n $ \a  ->
                            k $ ab a

instance Alternative f => Alternative (BindK f) where
  empty = BindK $ \_ -> empty
  BindK m <|> BindK n = BindK $ \k -> m k <|> n k

-- | Run a @'BindK' f@ computation with 'pure' as the final continuation.
lowerBindK :: Applicative f => BindK f a -> f a
lowerBindK (BindK k) = k pure

------------------------------------------------------------------------

-- | Combine two lists together alternating alternating from each and
-- starting with the first list.
interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave []     ys = ys

-- | "Fair" bind operation for lists.
(>>-) :: [a] -> (a -> [b]) -> [b]
[]     >>- _ = []
(x:xs) >>- f = interleave (f x) (xs >>- f)

-- | 'interleave' operation lifted to 'BindK'
(|||) :: BindK [] a -> BindK [] a -> BindK [] a
BindK m ||| BindK n = BindK (\k -> interleave (m k) (n k))

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
