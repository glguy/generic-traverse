module Boggle where

import Control.Lens
import Data.Functor.Kan.Rift    (Rift(..), runRift)
import Data.Functor.Yoneda      (Yoneda(..), liftYoneda, lowerYoneda)
import GHC.Generics             (Generic1, Rep1, to1, from1,
                                 M1(..), K1(..), Par1(..), (:*:)(..), (:+:)(..))


------------------------------------------------------------------------
-- Control.Lens.Traversal.confusing replacement starts here
------------------------------------------------------------------------

type Rift' f = Rift f f

data Boggle f a
  = Boggle (Yoneda f a) (Rift' (Yoneda f) a)
  | BPure a

-- | Optimize a Traversal by using the fmaps and left-associating the (<*>)s
boggling :: Applicative f => LensLike (Boggle f) s t a b -> LensLike f s t a b
boggling = auf boggled
{-# INLINE boggling #-}

boggled :: Applicative f => Iso (f a) (f b) (Boggle f a) (Boggle f b)
boggled = iso liftBoggle lowerBoggle
{-# INLINE boggled #-}

lowerBoggle :: Applicative f => Boggle f a -> f a
lowerBoggle (Boggle x _) = lowerYoneda x
lowerBoggle (BPure x)    = pure x
{-# INLINE lowerBoggle #-}

liftBoggle :: Applicative f => f a -> Boggle f a
liftBoggle fa = Boggle (liftYoneda fa) (liftRiftYoneda fa)
{-# INLINE liftBoggle #-}

-- | The natural transformation between @f@ and @'Rift'' ('Yoneda' f)@.
-- The key to this implementation is that it doesn't introduce any new
-- 'fmap' uses that would occur with a use of 'liftYoneda'.
liftRiftYoneda :: Applicative f => f a -> Rift' (Yoneda f) a
liftRiftYoneda fa = Rift (\(Yoneda k) -> Yoneda (\ab_r -> k (ab_r .) <*> fa))
{-# INLINE liftRiftYoneda #-}

instance Functor (Boggle f) where
  fmap f (Boggle x y)   = Boggle (fmap f x) (fmap f y)
  fmap f (BPure x)      = BPure (f x)
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Boggle f) where
  pure                          = BPure
  Boggle fy fr <*> Boggle _ x   = Boggle (runRift x fy) (fr <*> x)
  BPure f      <*> x            = fmap f x
  f            <*> BPure x      = fmap ($ x) f
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
