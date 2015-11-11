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
  -- * Abstractions
  , LensLike, Traversal, Traversal'
  , Apply(..)
  , ApWrap(..), liftApWrap, lowerApWrap
  -- * fmap fusion
  , MapK(..), liftMapK, lowerMapK, (<<$>)
  -- * fmap fusion with fmap id law
  , MapK1(..), liftMapK1, lowerMapK1
  -- * \<*\> fusion
  , ApK(..), (<<.>), liftApK, lowerApK
  -- * \<.\> fusion
  , ApK1(..), liftApK1, lowerApK1
  -- * pure fusion
  , PureK(..), liftPureK, lowerPureK
  ) where

infixl 4 <<$>, <<.>, <.>

type LensLike f s t a b = (a -> f b) -> (s -> f t)
type Traversal s t a b = forall f. Applicative f => LensLike f s t a b
type Traversal' s a = Traversal s s a a

-- | This class is a mid-point between 'Functor' and 'Applicative'
-- for types that support the '<*>' operation but not 'pure'
--
-- > ((.) <$> f <.> g) <.> x === f <.> (g <.> x)
class Functor f => Apply f where
  (<.>) :: f (a -> b) -> f a -> f b

------------------------------------------------------------------------

-- | In a perfect world, 'Apply' would be a super class of 'Applicative'.
-- This newtype implements an 'Apply' instance in terms of an underlying
-- 'Applicative' instance.
newtype ApWrap f a = ApWrap (f a)

liftApWrap :: f a -> ApWrap f a
liftApWrap = ApWrap

lowerApWrap :: ApWrap f a -> f a
lowerApWrap (ApWrap fa) = fa

instance Functor f => Functor (ApWrap f) where
  fmap f (ApWrap x) = ApWrap (f <$> x)

-- | @('<.>') = ('<*>')@
instance Applicative f => Apply (ApWrap f) where
  ApWrap f <.> ApWrap x = ApWrap (f <*> x)


------------------------------------------------------------------------

-- | This type fuses all uses of 'fmap' into a single use of 'fmap' on
-- the underlying 'Functor' @f@.
newtype MapK f a = MapK (forall b. (a -> b) -> f b)

liftMapK :: Functor f => f a -> MapK f a
liftMapK fa = MapK (<$> fa)

lowerMapK :: MapK f a -> f a
lowerMapK (MapK k) = k id

-- | Like '<$>' but removes the 'MapK'
(<<$>) :: (a -> b) -> MapK f a -> f b
f <<$> MapK x = x f

-- | Note: no underlying 'Functor' required
instance Functor (MapK f) where
  fmap f x = MapK (\z -> (z . f) <<$> x)
                -- z <$> (f <$> x)
                -- (z . f) <$> x

------------------------------------------------------------------------

-- | 'MapK1' extends 'MapK' to detect when a lift is immediately followed
-- by a lower. In this case no 'fmap' will be used at all!
data MapK1 f a = MapK1 (f a) (MapK f a)

liftMapK1 :: Functor f => f a -> MapK1 f a
liftMapK1 fa = MapK1 fa (liftMapK fa)

lowerMapK1 :: MapK1 f a -> f a
lowerMapK1 (MapK1 fa _) = fa

-- | Note: no underlying 'Functor' required
instance Functor (MapK1 f) where
  fmap f (MapK1 _ g) = MapK1 (f <<$> g) (f <$> g)

instance Apply f => Apply (MapK1 f) where
  MapK1 f g <.> MapK1 x _ = MapK1 (f <.> x) (MapK (\k -> (.) k <<$> g <.> x))
  -- k <$> (g <.> x)
  -- pure k <.> (g <.> x)
  -- pure (.) <.> pure k <.> g <.> x
  -- pure ((.) k) <.> g <.> x
  -- (.) k <$> g <.> x

------------------------------------------------------------------------

-- | 'ApK' provides an 'Apply' instance in terms of the underlying @f@'s
-- 'Apply' instance, but left-associates all '<.>'. Lowering this type
-- requires an 'Applicative' instance.
newtype ApK f a = ApK (forall b. f (a -> b) -> f b)

liftApK :: Apply f => f a -> ApK f a
liftApK fa = ApK (<.> fa)

lowerApK :: Applicative f => ApK f a -> f a
lowerApK fa = pure id <<.> fa

-- | Like '<.>' but removes 'ApK'
(<<.>) :: f (a -> b) -> ApK f a -> f b
fa <<.> ApK k = k fa

instance Functor f => Functor (ApK f) where
  fmap f x = ApK (\g -> (.f) <$> g <<.> x)
                -- g <.> (f <$> x)
                -- g <.> (pure f <.> x)
                -- pure (.) <.> g <.> pure f <.> x
                -- pure ($ f) <.> (pure (.) <.> g) <.> x
                -- pure (.) <.> pure ($ f) <.> pure (.) <.> g <.> x
                -- pure ((.) ($ f) (.)) <.> g <.> x
                -- (. f) <$> g <.> x

-- | Note that this 'Apply' instance only uses the underlying 'Functor'
instance Functor f => Apply (ApK f) where
  f <.> x = ApK (\g -> (.) <$> g <<.> f <<.> x)
                -- g <.> (f <.> x)
                -- pure (.) <.> g <.> f <.> x
                -- (.) <$> g <.> f <.> x

------------------------------------------------------------------------

-- | This type provides an 'Apply' instance in terms of the underlying @f@
-- type's 'Apply' instance, but it left-associates all uses of '<.>'
data ApK1 f a = ApK1 (f a) (ApK f a)

instance Functor f => Functor (ApK1 f) where
  fmap f (ApK1 x y) = ApK1 (f <$> x) (f <$> y)

-- | Note that this 'Apply' instance only uses the underlying 'Functor'
instance Functor f => Apply (ApK1 f) where
  ApK1 fl fr <.> ApK1 _ x = ApK1 (fl <<.> x) (fr <.> x)

liftApK1 :: Apply f => f a -> ApK1 f a
liftApK1 fa = ApK1 fa (liftApK fa)

lowerApK1 :: ApK1 f a -> f a
lowerApK1 (ApK1 fa _) = fa

------------------------------------------------------------------------

-- | 'PureK' lifts a type @f@ having an 'Apply' instance to a type
-- having an 'Applicative' instance. The 'Applicative' laws for 'pure'
-- are used to rewrite all uses of pure into either a single 'pure' or
-- into uses of 'fmap' where possible.
data PureK f a = Pure a | Dirty (f a)

lowerPureK :: Applicative f => PureK f a -> f a
lowerPureK (Pure a)   = pure a
lowerPureK (Dirty fa) = fa

liftPureK :: f a -> PureK f a
liftPureK = Dirty

instance Functor f => Functor (PureK f) where
  fmap f (Pure x)  = Pure (f x)
  fmap f (Dirty x) = Dirty (f <$> x)

instance Apply f => Apply (PureK f) where
  Dirty f <.> Dirty x = Dirty (f <.> x)
  Pure f  <.> x       = f <$> x
  f       <.> Pure x  = ($ x) <$> f

-- Note that this 'Applicative' instance only uses the underlying 'Apply'
instance Apply f => Applicative (PureK f) where
  pure = Pure
  (<*>) = (<.>)

-- | Transform the underlying type.
natPureK :: (f a -> g a) -> PureK f a -> PureK g a
natPureK f (Dirty fa) = Dirty (f fa)
natPureK _ (Pure a)   = Pure a

------------------------------------------------------------------------

-- | @'Boggle' f@ is isomorphic to @f@ up to the 'Applicative' laws.
-- Uses of '<$>' on this type are combined into a single use of '<$>'
-- on the underlying @f@ type. Uses of 'pure' are combined and transformed
-- to '<$>' where possible. Uses of '<*>' are reassociated to the left.
--
-- 'PureK' is on the outside because any use of 'pure' is immediately
-- intercepted and translated into 'fmap' when needed. 'ApK1' doesn't
-- even have/need an 'Applicative' instance.
--
-- 'ApK1' is next because it uses the 'Functor' instance from its underlying
-- type and we want the 'MapK' layer to intercept and fuse all of those 'fmap'
-- uses.
--
-- 'MapK1' is down toward the bottom of the stack to be able to fuse the uses
-- of 'fmap' from all the previous layers into one single use.
--
-- 'ApWrap' is at the very bottom. It only exists to provide an 'Apply'
-- instance to the underlying type @f@.
newtype Boggle f a = Boggle
  { unBoggle :: PureK (ApK1 (MapK1 (ApWrap f))) a }

instance Functor (Boggle f) where
  fmap f (Boggle x) = Boggle (f <$> x)
  {-# INLINE fmap #-}

instance Applicative (Boggle f) where
  pure = \x -> Boggle (pure x)
  {-# INLINE pure #-}
  Boggle x <*> Boggle y = Boggle (x <*> y)
  {-# INLINE (<*>) #-}

liftBoggle :: Applicative f => f a -> Boggle f a
liftBoggle = Boggle . liftPureK . liftApK1 . liftMapK1 . liftApWrap
{-# INLINE liftBoggle #-}

-- | 'lowerBoggle' lowers the 'ApK1' and 'MapK' layers first before lowering
-- the 'PureK' layer. This ensures that any 'fmap' uses in the 'PureK' layer
-- are intercepted by the 'MapK' layer, but the final (possible) use of 'pure'
-- in the case that 'pure' is going to be used will happen in the underlying
-- @f@ type!
lowerBoggle :: Applicative f => Boggle f a -> f a
lowerBoggle
  = lowerPureK . natPureK (lowerApWrap . lowerMapK1 . lowerApK1) . unBoggle
{-# INLINE lowerBoggle #-}

-- | Optimize a 'Traversal' by fusing the '<$>'s and left-associating the '<*>'s
boggling :: Applicative f => LensLike (Boggle f) s t a b -> LensLike f s t a b
boggling l = \f x -> lowerBoggle (l (liftBoggle . f) x)
{-# INLINE boggling #-}
