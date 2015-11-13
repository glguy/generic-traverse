{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funfolding-use-threshold=5000 -funfolding-creation-threshold=1500 #-}

-- | This module demonstrates how using 'Boogle' can enable GHC to
-- generate more efficient code for a 'Traversal'.
module Boggle.Demo (Demo(..), NonEmpty(..), badTraversal, goodTraversal) where

import Boggle                   (Traversal', boggling)
import Data.Traversable.Generic (genericTraverse)

import Data.Traversable         (fmapDefault, foldMapDefault)
import GHC.Generics             (Generic1)

------------------------------------------------------------------------
-- Example use of 'boggling' operator to optimize a Traversal
------------------------------------------------------------------------

-- | Example type featuring multiple constructors, empty constructors,
-- constructors with many fields, and some fields in the left-most
-- position (which would generally cause a use of 'pure')
data Demo a = Zero | One | Two | Three | Four Int a a a
  deriving (Show, Generic1)

instance Functor     Demo where fmap    f = fmapDefault f
instance Foldable    Demo where foldMap f = foldMapDefault f
instance Traversable Demo where traverse  = genericTraverse

-- | This traversal is written in a non-normalized way.
badTraversal :: Traversal' (Int,Int,Int) Int
badTraversal f (x,y,z) =
  pure (\x' (y',z') -> (x',y',z')) <*> f x <*> ((,) <$> f y <*> f z)

-- liftA2 (\x' (y',z') -> (x',y',z')) (f x) (liftA2 (,) (f y) (f z))
-- works equally well with liftA2 directly

-- | This traversal is derived from 'badTraversal' but has the same
-- implementation as one written in a normalized way.
goodTraversal :: Traversal' (Int,Int,Int) Int
goodTraversal = boggling badTraversal
  -- generated code is \f (x,y,z) -> (,,) <$> f x <*> f y <*> f z

-- | This type exists to demonstrate how the technique works on
-- recursive data types.
data NonEmpty a = Cons a (Maybe (NonEmpty a)) deriving Generic1

instance Functor     NonEmpty where fmap    f = fmapDefault f
instance Foldable    NonEmpty where foldMap f = foldMapDefault f
instance Traversable NonEmpty where traverse  = genericTraverse
