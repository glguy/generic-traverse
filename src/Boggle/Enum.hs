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
  , BindK(..), lowerBindK, liftBindK
    -- * List helper functions
  , Search(..)
    -- * Example
  , Demo(..), demos
  ) where

import Control.Applicative      (Alternative(..))
import Control.Monad            (ap, liftM, MonadPlus)
import Data.Void                (Void)
import GHC.Generics

-- | This class provides a list of all the elements of a type.
class Enumerate a where
  enumerate :: [a]

  default enumerate :: (Generic a, GEnumerate (Rep a)) => [a]
  enumerate = search (lowerBindK (to <$> genumerate))


class GEnumerate g where
  genumerate :: BindK Search (g a)

instance GEnumerate f => GEnumerate (M1 i c f) where
  genumerate = M1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :+: g) where
  genumerate = L1 <$> genumerate <|> R1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :*: g) where
  genumerate = (:*:) <$> genumerate <*> genumerate

instance Enumerate b => GEnumerate (K1 i b) where
  genumerate = K1 <$> liftBindK (Search enumerate)

instance GEnumerate U1 where
  genumerate = pure U1

instance GEnumerate V1 where
  genumerate = empty

------------------------------------------------------------------------

-- | Local implementation of @Codensity@ type from @kan-extensions@.
-- This type captures the concept of a partially applied '>>=' function.
newtype BindK f a = BindK { runBindK :: forall b. (a -> f b) -> f b }

instance Functor (BindK f) where
  fmap = liftM

instance Applicative (BindK f) where
  pure x = BindK $ \k -> k x
  (<*>)  = ap

instance Alternative f => Alternative (BindK f) where
  empty = BindK $ \_ -> empty
  BindK m <|> BindK n = BindK $ \k -> m k <|> n k
  {-# INLINE (<|>) #-}

instance Monad (BindK f) where
  BindK m >>= f = BindK $ \k -> m $ \a -> runBindK (f a) k

instance Alternative f => MonadPlus (BindK f)

-- | Run a @'BindK' f@ computation with 'pure' as the final continuation.
lowerBindK :: Applicative f => BindK f a -> f a
lowerBindK (BindK k) = k pure

liftBindK :: Monad f => f a -> BindK f a
liftBindK fa = BindK (fa >>=)

------------------------------------------------------------------------

-- | 'Search' provides a 'Monad' instance implementing fair backtracking.
-- It satisfies the 'Monad' laws using set equality rather than strict
-- equality.
newtype Search a = Search { search :: [a] }

instance Functor Search where
  fmap = liftM

instance Applicative Search where
  pure x = Search [x]
  (<*>)  = ap

-- | Interleaving bind
instance Monad Search where
  Search []     >>= _ = empty
  Search (x:xs) >>= f = f x <|> (Search xs >>= f)

-- | Interleaving of two searches
instance Alternative Search where
  empty                = Search empty
  Search (x:xs) <|> ys = Search (x : search (ys <|> Search xs))
  Search []     <|> ys = ys

instance MonadPlus Search

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
