{-# OPTIONS -funfolding-use-threshold=1000 -funfolding-creation-threshold=1000 #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Data.Functor.Rep.GenericsLens where

import Control.Lens
import Control.Monad.Fix
import Control.Monad.Zip
import Data.Distributive
import Data.Functor.Rep
import Data.Traversable
import Data.Traversable.Generic
import GHC.Generics
import GHC.Generics.Lens

data V3 a = V3 a a a deriving (Show, Generic1)

instance Distributive  V3 where distribute  = distributeRep
instance Foldable      V3 where foldMap     = foldMapDefault
instance Traversable   V3 where traverse    = genericTraverse
instance Functor       V3 where fmap        = fmapDefault
instance Applicative   V3 where pure        = pureRep
                                (<*>)       = apRep
instance Monad         V3 where (>>=)       = bindRep
instance MonadZip      V3 where mzipWith    = mzipWithRep
instance MonadFix      V3 where mfix        = mfixRep
instance Representable V3 where type Rep V3 = E V3
                                tabulate    = genericTabulate
                                index       = genericIndex






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
  gtabulate f = gtabulate (f . comp _1')
            :*: gtabulate (f . comp _2')

-- | No fields
instance GTabulate U1 where
  gtabulate _ = U1

-- | Single field using type's rightmost parameter
instance GTabulate Par1 where
  gtabulate f = Par1 (f (E _Par1))

-- Helper function for composing a 'Lens' and 'E'
comp :: (forall a. Lens' (u a) (t a)) -> E t -> E u
comp l e = E (l . eLens e)
