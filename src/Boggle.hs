-- | This module implements the 'Boggle' type which exists for its
-- 'Applicative' instance that takes advantage of the laws of the
-- 'Applicative' class to rearrange the applications of the underlying
-- type's 'Applicative' instance. These transformations collect all of
-- the pure values using in 'pure' and 'fmap' calls into a single place
-- which enables GHC to aggressively optimize them.
module Boggle
  ( Boggle
  , boggling
  , liftBoggle, lowerBoggle, boggled
  -- * Implementation details
  , liftRiftYoneda
  , Rift'
  ) where

import Control.Lens
import Data.Functor.Kan.Rift    (Rift(..), runRift)
import Data.Functor.Yoneda      (Yoneda(..), liftYoneda, lowerYoneda)

------------------------------------------------------------------------
-- Control.Lens.Traversal.confusing replacement starts here
------------------------------------------------------------------------

-- | When the first two arguments to 'Rift' match and have an 'Applicative'
-- instance, then 'Rift' has an 'Applicative instance.
type Rift' f = Rift f f

-- | @'Boggle' f@ is isomorphic to @f@ up to the 'Applicative' laws.
-- Uses of 'fmap' on this type are combined into a single use of 'fmap'
-- on the underlying @f@ type. Uses of 'pure' are combined and transformed
-- to 'fmap' where possible. Uses of '<*>' are reassociated to the left.
data Boggle f a
  = Boggle (Yoneda f a) (Rift' (Yoneda f) a)
  | BPure a

-- | Optimize a Traversal by using the fmaps and left-associating the (<*>)s
boggling :: Applicative f => LensLike (Boggle f) s t a b -> LensLike f s t a b
boggling = auf boggled
{-# INLINE boggling #-}

-- | The isomorphism between @f@ and @'Boggle' f@ which holds up to the
-- Applicative laws as realized by 'liftBoggle' and 'lowerBoggle'.
boggled :: Applicative f => Iso (f a) (f b) (Boggle f a) (Boggle f b)
boggled = iso liftBoggle lowerBoggle
{-# INLINE boggled #-}

-- | The natural transformation from @'Boggle' f@ to @f@.
lowerBoggle :: Applicative f => Boggle f a -> f a
lowerBoggle (Boggle x _) = lowerYoneda x
lowerBoggle (BPure x)    = pure x
{-# INLINE lowerBoggle #-}

-- | The natural transformation from @f@ to @'Boggle' f@.
liftBoggle :: Applicative f => f a -> Boggle f a
liftBoggle fa = Boggle (liftYoneda fa) (liftRiftYoneda fa)
{-# INLINE liftBoggle #-}

-- | The natural transformation between @f@ and @'Rift'' ('Yoneda' f)@.
-- The key to this implementation is that it doesn't introduce any new
-- 'fmap' uses that would occur with a use of 'liftYoneda' directly.
--
-- @
-- 'Data.Functor.Kan.Rift.liftRift' ('liftYoneda' x)
-- \\a -> a            '<*>' 'liftYoneda' x
-- \\a -> \f -> a (f '.') '<*>' 'liftYoneda' x 'id'
-- \\a -> \f -> a (f '.') '<*>' (\\g -> 'fmap' g x) 'id'
-- \\a -> \f -> a (f '.') '<*>' 'fmap' 'id' x
-- \\a -> \f -> a (f '.') '<*>' x
-- @
liftRiftYoneda :: Applicative f => f a -> Rift' (Yoneda f) a
liftRiftYoneda fa = Rift (\(Yoneda k) -> Yoneda (\ab_r -> k (ab_r .) <*> fa))
{-# INLINE liftRiftYoneda #-}



instance Functor (Boggle f) where
  fmap f (Boggle x y)   = Boggle (fmap f x) (fmap f y)
  fmap f (BPure x)      = BPure (f x)
  {-# INLINE fmap #-}

-- | This instance takes advantage of the Applicative laws in the underlying
-- @'Applicative' f@ to reassociate '<*>' and to combine uses of 'pure'.
instance Applicative (Boggle f) where
  pure                          = BPure
  Boggle fy fr <*> Boggle _ x   = Boggle (runRift x fy) (fr <*> x)
  BPure f      <*> x            = fmap f x
  f            <*> BPure x      = fmap ($ x) f
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
