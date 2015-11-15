{-# OPTIONS -funfolding-use-threshold=1000 -funfolding-creation-threshold=1000 #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Functor.Rep.GenericLens
  ( -- * Generic implementation of Representable class
    genericTabulate, genericIndex, E(..)
    -- * Implementation class
  , GTabulate(..)
    -- * Example
  , V3(..), v3tabulate, v3index
  ) where

import Boggle (MapK, lowerMapK, liftMapK)
import Control.Applicative (Const(..))
import GHC.Generics

data V3 a = V3 a a a deriving (Show, Generic1)

v3tabulate :: (E V3 -> a) -> V3 a
v3tabulate = genericTabulate

v3index :: V3 a -> E V3 -> a
v3index = genericIndex






-- Indexes as used in the Linear package
-- An @'E' t@ is an index into the structure
newtype E t = E { eLens :: forall a. Lens' (t a) a }

-- This is going to require some inling effort on GHC's part, more than
-- it's usually willing to do. The use of 'fusing' from lens package
-- ensures that the generics can fuse.
genericTabulate :: (Generic1 f, GTabulate (Rep1 f)) => (E f -> a) -> f a
genericTabulate f = to1 (gtabulate (\e -> f (E (fusing (generic1 . eLens e)))))

genericIndex :: t a -> E t -> a
genericIndex x l = view (eLens l) x

-- | Class for deriving generic 'tabulate' implementations where the indexes are
-- lenses into the structure.
class GTabulate f where
  gtabulate :: (E f -> a) -> f a

-- | Ignore metadata
instance GTabulate f => GTabulate (M1 i c f) where
  gtabulate f = M1 (gtabulate (f . comp _M1))

-- | Multiple fields
instance (GTabulate f, GTabulate g) => GTabulate (f :*: g) where
  gtabulate f = gtabulate (f . comp _1)
            :*: gtabulate (f . comp _2)

-- | No fields
instance GTabulate U1 where
  gtabulate _ = U1

-- | Single field using type's rightmost parameter
instance GTabulate Par1 where
  gtabulate f = Par1 (f (E _Par1))

-- Helper function for composing a 'Lens' and 'E'
comp :: (forall a. Lens' (u a) (t a)) -> E t -> E u
comp l e = E (l . eLens e)

------------------------------------------------------------------------
-- Local copy of lens definitions
------------------------------------------------------------------------

type LensLike' f s a = (a -> f a) -> s -> f s
type Lens' s a = forall f. Functor f => LensLike' f s a

generic1 :: Generic1 f => Lens' (f a) (Rep1 f a)
generic1 f x = to1 <$> f (from1 x)

view :: LensLike' (Const a) s a -> s -> a
view l x = getConst (l Const x)

fusing :: Functor f => LensLike' (MapK f) s a -> LensLike' f s a
fusing l f x = lowerMapK (l (liftMapK . f) x)

_Par1 :: Lens' (Par1 a) a
_Par1 f (Par1 x) = Par1 <$> f x

_1 :: Lens' ((f :*: g) a) (f a)
_1 f (x :*: y) = (:*: y) <$> f x

_2 :: Lens' ((f :*: g) a) (g a)
_2 f (x :*: y) = (x :*:) <$> f y

_M1 :: Lens' (M1 i c f a) (f a)
_M1 f (M1 x) = M1 <$> f x
