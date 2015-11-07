{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funfolding-use-threshold=500 #-}

-- | This module demonstrates how using 'Boogle' can enable GHC to
-- generate more efficient code for a 'Traversal'.
module BoggleDemo (Demo(..), Stream(..), badTraversal, goodTraversal) where

import Boggle                   (boggling)
import Data.Traversable.Generic (genericTraverse)

import Control.Lens             (Traversal')
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
  -- The generated code is
  -- \f a ->
  --   case a of
  --     One -> pure Zero
  --     Four v w x y z -> Four v <$> f w <*> f x <*> f y <*> f z

-- | This traversal is written in a non-normalized way.
badTraversal :: Traversal' (Int,Int,Int) Int
badTraversal f (x,y,z) =
  pure (\x' (y',z') -> (x',y',z')) <*> f x <*> ((,) <$> f y <*> f z)

{- RULES
  "app assoc" forall a b c. a <*> (b <*> c) = fmap (.) a <*> b <*> c ;
  "pure/fmap" forall f m. pure f <*> m = fmap f m ;
  "ap/pure" forall a b. a <*> pure b = fmap ($ b) a ;
  "fmap/fmap" forall f g x. fmap f (fmap g x) = fmap (f . g) x ;
  "ap/fmap" forall f x y. x <*> fmap f y = fmap (. f) x <*> y ;
  -}

-- | This traversal is derived from 'badTraversal' but has the same
-- implementation as one written in a normalized way.
goodTraversal :: Traversal' (Int,Int,Int) Int
goodTraversal = boggling badTraversal
  -- generated code is \f (x,y,z) -> (,,) <$> f x <*> f y <*> f z

-- | This type exists to demonstrate how the technique works on
-- recursive data types.
data Stream a = Cons a (Maybe (Stream a)) deriving Generic1

instance Functor     Stream where fmap    f = fmapDefault f
instance Foldable    Stream where foldMap f = foldMapDefault f
instance Traversable Stream where traverse  = genericTraverse
