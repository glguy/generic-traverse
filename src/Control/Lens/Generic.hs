{-# LANGUAGE
 DataKinds, TypeFamilies,
 TypeOperators,
 MultiParamTypeClasses, UndecidableInstances, FlexibleContexts, FlexibleInstances,
 ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

{-# LANGUAGE DeriveGeneric #-}

module Control.Lens.Generic (genericOptic) where

import Control.Lens hiding (from,to)
import GHC.TypeLits
import Data.Kind
import GHC.Generics
import GHC.Generics.Lens
import Boggle (boggling)

------------------------------------------------------------------------
-- Class for generically deriving lenses
------------------------------------------------------------------------

type family Cx p t :: Constraint where
  Cx 'Skip        t = Applicative t
  Cx ('Both f g)  t = (Cx f t, Cx g t)
  Cx ('GoLeft f)  t = Cx f t
  Cx ('GoRight f) t = Cx f t
  Cx ('Pass f)    t = Cx f t
  Cx 'End         t = ()



-- | Generic implementation of 'Lens' and 'Traversal' given a field
-- name, the source and target generic representations, and the
-- source and target types of the thing being focused by the optic.
class GOptic n f g a b where
  goptic :: (Functor t, Cx n t) => LensLike t (f x) (g x) a b

instance f ~ g => GOptic 'Skip f g a b where
  goptic = ignored
  {-# Inline goptic #-}

instance GOptic p f g a b => GOptic ('Pass p) (M1 i c f) (M1 j d g) a b where
  goptic = _M1 . goptic @p
  {-# Inline goptic #-}

instance (GOptic p f1 g1 a b, GOptic q f2 g2 a b) =>
    GOptic ('Both p q) (f1 :+: f2) (g1 :+: g2) a b where
  goptic f (L1 x) = L1 <$> goptic @p f x
  goptic f (R1 x) = R1 <$> goptic @q f x
  {-# Inline goptic #-}

instance (x ~ x', GOptic p f g a b) => GOptic ('GoLeft p) (f :*: x) (g :*: x') a b where
  goptic = _1 . goptic @p
  {-# Inline goptic #-}

instance (x ~ x', GOptic p f g a b) => GOptic ('GoRight p) (x :*: f) (x' :*: g) a b where
  goptic = _2 . goptic @p
  {-# Inline goptic #-}

instance (a ~ a', b ~ b') => GOptic p (K1 i a) (K1 i b) a' b' where
  goptic = _K1
  {-# Inline goptic #-}


data Path = Pass Path | End | Skip | GoLeft Path | GoRight Path | Both Path Path

type family OrElse m n where
  OrElse 'Skip 'Skip = 'Skip
  OrElse 'Skip y     = 'GoRight y
  OrElse x     y     = 'GoLeft x

type family Check m where
  Check 'Skip = 'Skip
  Check x     = 'Pass x

type family Find (s :: Symbol) (hay :: * -> *) :: Path where
  Find s (D1 c f) = 'Pass (Find s f)
  Find s (C1 c f) = Check (Find s f)
  Find s (S1 ('MetaSel ('Just s) x y z) f) = 'Pass (Find s f)
  Find s (S1 i f) = 'Skip

  Find s (f :+: g) = 'Both (Find s f) (Find s g)
  Find s V1        = 'Skip

  Find s (x :*: y) = Find s x `OrElse` Find s y
  Find s U1        = 'Skip

  Find s (K1 i a)  = 'End


-- | More polymorphic than your standard 'generic'
generic' :: (Generic a, Generic b) => Iso a b (Rep a x) (Rep b y)
generic' = iso from to
{-# Inline generic' #-}



-- | Generically compute either a 'Lens' or a 'Traversal' based
-- on record field name. To construct a 'Lens', the field name
-- must appear in all of the data constructors of the target type.
genericOptic ::
  forall n s t a b f.
  ( Generic s, Generic t
  , GOptic (Find n (Rep s)) (Rep s) (Rep t) a b
  , Cx (Find n (Rep s)) f
  , Functor f) =>
  LensLike f s t a b
genericOptic = generic' . goptic @(Find n (Rep s))
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
lensA = fusing (genericOptic @"fieldA")

traversalB :: Traversal (Example1 a b) (Example1 a b') b b'
traversalB = boggling (genericOptic @"fieldB")

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
emptyTraversal = genericOptic @""
