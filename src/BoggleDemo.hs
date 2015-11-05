{-# LANGUAGE DeriveGeneric #-}

module BoggleDemo (Demo(..), goodTraversal) where

import Boggle                   (boggling)
import Data.Traversable.Generic (genericTraverse)

import Control.Lens             (Traversal')
import Data.Traversable         (foldMapDefault, fmapDefault)
import GHC.Generics             (Generic1)

------------------------------------------------------------------------
-- Example use of 'boggling' operator to optimize a Traversal
------------------------------------------------------------------------

data Demo a = Zero | Four Int a a a a
  deriving (Show, Generic1)

instance Functor     Demo where fmap    f = fmapDefault f
instance Foldable    Demo where foldMap f = foldMapDefault f
instance Traversable Demo where traverse = boggling genericTraverse
  -- The generated code is
  -- \f a ->
  --   case a of
  --     One -> pure Zero
  --     Four v w x y z -> Four v <$> f w <*> f x <*> f y <*> f z

badTraversal :: Traversal' (Int,Int,Int) Int
badTraversal f (x,y,z) =
  pure (\x' (y',z') -> (x',y',z')) <*> f x <*> ((,) <$> f y <*> f z)

goodTraversal :: Traversal' (Int,Int,Int) Int
goodTraversal = boggling badTraversal
  -- generated code is \f (x,y,z) -> (,,) <$> f x <*> f y <*> f z
