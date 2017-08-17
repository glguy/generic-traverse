{-# LANGUAGE
 DataKinds, TypeFamilies,
 MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, FlexibleInstances,
 TypeOperators, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

{-# LANGUAGE DeriveGeneric #-}

module Control.Lens.Generic where

import Control.Lens hiding (from,to)
import Data.Type.Bool
import Data.Kind
import GHC.Generics
import GHC.Generics.Lens
import Boggle (Boggle, boggling)
import Data.Functor.Yoneda

------------------------------------------------------------------------
-- Class for generically deriving lenses
------------------------------------------------------------------------


-- | Generic implementation of 'Lens' and 'Traversal' given a field
-- name, the source and target generic representations, and the
-- source and target types of the thing being focused by the optic.
class Functor t => GOptic (p :: Path) t f g a b where
  goptic :: LensLike t (f x) (g x) a b

instance (Applicative t, f ~ g) =>
         GOptic 'Skip t f g a b where
  goptic = ignored
  {-# Inline goptic #-}

instance GOptic p t f g a b =>
         GOptic ('Pass p) t (M1 i c f) (M1 j d g) a b where
  goptic = _M1 . goptic @p
  {-# Inline goptic #-}

instance (GOptic p t f1 g1 a b, GOptic q t f2 g2 a b) =>
         GOptic ('Both p q) t (f1 :+: f2) (g1 :+: g2) a b where
  goptic f (L1 x) = L1 <$> goptic @p f x
  goptic f (R1 x) = R1 <$> goptic @q f x
  {-# Inline goptic #-}

instance (x ~ x', GOptic p t f g a b) =>
         GOptic ('GoLeft p) t (f :*: x) (g :*: x') a b where
  goptic = _1 . goptic @p
  {-# Inline goptic #-}

instance (x ~ x', GOptic p t f g a b) =>
         GOptic ('GoRight p) t (x :*: f) (x' :*: g) a b where
  goptic = _2 . goptic @p
  {-# Inline goptic #-}

instance (a ~ a', b ~ b', Functor t) =>
         GOptic 'End t (K1 i a) (K1 i b) a' b' where
  goptic = _K1
  {-# Inline goptic #-}

------------------------------------------------------------------------
-- Optimizer selection
------------------------------------------------------------------------

class Optimizer (x :: Bool) where
  optimizer ::
    If x Applicative Functor f =>
    LensLike (If x Boggle Yoneda f) s t a b -> LensLike f s t a b

instance Optimizer 'True where
  optimizer = boggling

instance Optimizer 'False where
  optimizer = fusing

------------------------------------------------------------------------
-- Instance resolution logic
------------------------------------------------------------------------

data Path = Pass Path | End | Skip | GoLeft Path | GoRight Path | Both Path Path

type family OrElse m n where
  OrElse 'Skip 'Skip = 'Skip
  OrElse 'Skip y     = 'GoRight y
  OrElse x     y     = 'GoLeft x

type family Check m where
  Check 'Skip = 'Skip
  Check x     = 'Pass x

type family Both' m n where
  Both' 'Skip 'Skip = 'Skip
  Both' x     y     = 'Both x y

type family Find s f where
  Find s (D1 c f) = 'Pass (Find s f)
  Find s (C1 c f) = Check (Find s f)
  Find s (S1 ('MetaSel ('Just s) x y z) f) = 'Pass (Find s f)
  Find s (S1 i f) = 'Skip

  Find s (f :+: g) = Find s f `Both'` Find s g
  Find s V1        = 'Skip

  Find s (x :*: y) = Find s x `OrElse` Find s y
  Find s U1        = 'Skip
  Find s (K1 i a)  = 'End

type family HasSkip p where
  HasSkip 'Skip        = 'True
  HasSkip 'End         = 'False
  HasSkip ('Both x y)  = HasSkip x || HasSkip y
  HasSkip ('Pass x)    = HasSkip x
  HasSkip ('GoLeft x)  = HasSkip x
  HasSkip ('GoRight x) = HasSkip x

------------------------------------------------------------------------

-- | More polymorphic than your standard 'generic'
generic' :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b y)
generic' = iso from to
{-# Inline generic' #-}



-- | Generically compute either a 'Lens' or a 'Traversal' based
-- on record field name. To construct a 'Lens', the field name
-- must appear in all of the data constructors of the target type.
basicGenericOptic ::
  forall n s t a b f p.
  ( p ~ Find n (Rep s)
  , Generic s, Generic t
  , GOptic p f (Rep s) (Rep t) a b
  ) =>
  LensLike f s t a b
basicGenericOptic = generic' . goptic @p
{-# Inline basicGenericOptic #-}

genericOptic ::
  forall n s t a b f p x.
  ( p ~ Find n (Rep s)
  , x ~ HasSkip p
  , Generic s, Generic t
  , GOptic p (If x Boggle Yoneda f) (Rep s) (Rep t) a b
  , If x Applicative Functor f
  , Optimizer x
  ) =>
  LensLike f s t a b
genericOptic = optimizer @(HasSkip p) (basicGenericOptic @n)
{-# Inline genericOptic #-}


------------------------------------------------------------------------
-- Example use-case
------------------------------------------------------------------------

data Example a = MkExample
  { field1 :: Int
  , field2 :: Char
  , field3 :: [a]
  }
  deriving Generic

lens1 :: Lens' (Example a) Int
lens1 = genericOptic @"field1"

lens2 :: Lens' (Example a) Char
lens2 = genericOptic @"field2"

lens3 :: Lens (Example a) (Example b) [a] [b]
lens3 = genericOptic @"field3"

data Example1 a b
  = Example1A { fieldA :: [a] , fieldB :: b }
  | Example1B { fieldA :: [a] }
  deriving Generic

lensA :: Lens (Example1 a b) (Example1 a' b) [a] [a']
lensA = genericOptic @"fieldA"

traversalB :: Traversal (Example1 a b) (Example1 a b') b b'
traversalB = genericOptic @"fieldB"

data HasPhantom a b = HasPhantom { hasPhantom :: b }
  deriving Generic

lensP :: Lens (HasPhantom a b) (HasPhantom a' b') b b'
lensP = genericOptic @"hasPhantom"

data Maybe' a = Just' { fromJust' :: a } | Nothing'
  deriving Generic

justTraversal :: Traversal (Maybe' a) (Maybe' b) a b
justTraversal = genericOptic @"fromJust'"

data Empty deriving Generic

emptyTraversal :: Traversal Empty Empty a b
emptyTraversal = genericOptic -- It doesn't matter what the symbol is!
