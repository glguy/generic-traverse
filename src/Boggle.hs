{-# LANGUAGE RankNTypes #-}

-- | This module implements the 'Boggle' type which exists for its
-- 'Applicative' instance that takes advantage of the laws of the
-- 'Applicative' class to rearrange the applications of the underlying
-- type's 'Applicative' instance. These transformations collect all of
-- the pure values using in 'pure' and 'fmap' calls into a single place
-- which enables GHC to aggressively optimize them.
module Boggle
  ( Boggle(..)
  , boggling
  , liftBoggle, lowerBoggle
  , LensLike, Traversal, Traversal'
  -- * Implementation details
  , MapK(..), liftMapK, lowerMapK
  , ApK(..), (<<.>), (<.>)
  ) where

type LensLike f s t a b = (a -> f b) -> (s -> f t)
type Traversal s t a b = forall f. Applicative f => LensLike f s t a b
type Traversal' s a = Traversal s s a a

newtype MapK f a = MapK (forall b. (a -> b) -> f b)

instance Functor (MapK f) where
  fmap f (MapK g) = MapK (\z -> g (z . f))

liftMapK :: Functor f => f a -> MapK f a
liftMapK fa = MapK (<$> fa)

lowerMapK :: MapK f a -> f a
lowerMapK (MapK k) = k id

------------------------------------------------------------------------

newtype ApK f a = ApK (forall b. f (a -> b) -> f b)

(<<.>) :: f (a -> b) -> ApK f a -> f b
fa <<.> ApK k = k fa
infixl 4 <<.>

(<.>) :: Functor f => ApK f (a -> b) -> ApK f a -> ApK f b
f <.> x = ApK (\z -> (.) <$> z <<.> f <<.> x)
                -- ≡ z <.> (f <.> x)
infixl 4 <.>

instance Functor f => Functor (ApK f) where
  fmap f g = ApK (\z -> (.f) <$> z <<.> g)

-- | The natural transformation between @f@ and @'ApK'' ('MapK' f)@.
-- The key to this implementation is that it doesn't introduce any new
-- 'fmap' uses that would occur with a use of 'liftMapK' directly.
--
-- @
-- ('<*>' 'liftMapK' x)
-- \\a -> a            '<*>' 'liftMapK' x
-- \\a -> \\f -> a (f '.') '<*>' 'liftMapK' x 'id'
-- \\a -> \\f -> a (f '.') '<*>' (\\g -> 'fmap' g x) 'id'
-- \\a -> \\f -> a (f '.') '<*>' 'fmap' 'id' x
-- \\a -> \\f -> a (f '.') '<*>' x
-- @
liftApKMapK :: Applicative f => f a -> ApK (MapK f) a
liftApKMapK fa = ApK (\(MapK k) -> MapK (\ab_r -> k (ab_r .) <*> fa))
{-# INLINE liftApKMapK #-}

------------------------------------------------------------------------

-- | @'Boggle' f@ is isomorphic to @f@ up to the 'Applicative' laws.
-- Uses of '<$>' on this type are combined into a single use of '<$>'
-- on the underlying @f@ type. Uses of 'pure' are combined and transformed
-- to '<$>' where possible. Uses of '<*>' are reassociated to the left.
data Boggle f a
  = Pure a
  | Nonpure (MapK f a) (ApK (MapK f) a)
    -- ^ invariant: first ≡ lowerRift second

-- | Optimize a 'Traversal' by fusing the '<$>'s and left-associating the
-- '<*>'s
boggling :: Applicative f => LensLike (Boggle f) s t a b -> LensLike f s t a b
boggling l = \f x -> lowerBoggle (l (liftBoggle . f) x)
{-# INLINE boggling #-}

-- | The natural transformation from @'Boggle' f@ to @f@.
lowerBoggle :: Applicative f => Boggle f a -> f a
lowerBoggle (Nonpure x _) = lowerMapK x
lowerBoggle (Pure x)      = pure x
{-# INLINE lowerBoggle #-}

-- | The natural transformation from @f@ to @'Boggle' f@.
liftBoggle :: Applicative f => f a -> Boggle f a
liftBoggle fa = Nonpure (liftMapK fa) (liftApKMapK fa)
{-# INLINE liftBoggle #-}

-- | Note: this instance does not rely on a 'Functor' instance for @f@
instance Functor (Boggle f) where
  fmap f (Nonpure x y) = Nonpure (f <$> x) (f <$> y)
  fmap f (Pure x)      = Pure (f x)
  {-# INLINE fmap #-}

-- | Note: this instance does not rely on an 'Applicative' instance for @f@
instance Applicative (Boggle f) where
  pure                          = Pure
  Nonpure f1 f2 <*> Nonpure _ x = Nonpure (f1 <<.> x) (f2 <.> x)
  Pure f        <*> x           = f <$> x
  f             <*> Pure x      = ($ x) <$> f
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
