{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}

module Control.Lens.Generic (genericOptic) where

import Control.Lens hiding (from,to)
import GHC.TypeLits
import Data.Kind
import GHC.OverloadedLabels
import GHC.Generics
import GHC.Generics.Lens
import Boggle (boggling)

------------------------------------------------------------------------
-- Class for generically deriving lenses
------------------------------------------------------------------------

-- | If the field @n@ exists in all of the constructors represented by
-- the GHC.Generics Rep for a type, this computes the constraint @Functor t@,
-- otherwise it computes the constraint @Applicative t@.
type family GOpticCx n f t :: Constraint where
  GOpticCx n (D1 c f)  t = GOpticCx n f t
  GOpticCx n (f :+: g) t = (GOpticCx n f t, GOpticCx n g t)
  GOpticCx n (C1 c f)  t = GOpticConCx (Find n f) t

type family GOpticConCx p where
  GOpticConCx 'Nothing  = Applicative
  GOpticConCx ('Just x) = Functor


-- | Generic implementation of 'Lens' and 'Traversal' given a field
-- name, the source and target generic representations, and the
-- source and target types of the thing being focused by the optic.
class GOptic (n :: Symbol) f g a b where
  goptic :: (Functor t, GOpticCx n f t) => LensLike t (f x) (g x) a b

instance GOptic n f g a b => GOptic n (D1 c f) (D1 d g) a b where
  goptic = _M1 . goptic @n
  {-# Inline goptic #-}

instance (GOptic n f1 g1 a b, GOptic n f2 g2 a b) =>
    GOptic n (f1 :+: f2) (g1 :+: g2) a b where
  goptic f (L1 x) = L1 <$> goptic @n f x
  goptic f (R1 x) = R1 <$> goptic @n f x
  {-# Inline goptic #-}

instance GOpticCon (Find n f) f g a b => GOptic n (C1 c f) (C1 d g) a b where
  goptic = _M1 . gopticCon @(Find n f)
  {-# Inline goptic #-}



-- | This class dispatches either to a generic lens implementation
-- or the 'ignored' traversal depending on whether or not the target
-- field is a member of this constructor
class GOpticCon p f g a b where
  gopticCon :: GOpticConCx p t => LensLike t (f x) (g x) a b

-- | Constructors that do /not/ have this field are ignored
instance f ~ g => GOpticCon 'Nothing f g a b where
  gopticCon = ignored
  {-# Inline gopticCon #-}

-- | Constructors that do have this field require a lens
instance (GLens p f g, PathType p f ~ a, PathType p g ~ b) =>
         GOpticCon ('Just p) f g a b where
  gopticCon = glens @p
  {-# Inline gopticCon #-}



-- | This class generically constructs lenses at the level
-- of fields in a constructor. The 'Path' parameter must
-- correspond to a field in the generic representation of
-- the data type. The parameters @f@ and @g@ are allowed
-- to vary just enough to allow the type of the focused field
-- to change.
class GLens (p :: Path) f g where
  glens :: Lens (f a) (g a) (PathType p f) (PathType p g)

instance GLens p f g => GLens ('GoLeft p) (f :*: x) (g :*: x) where
  glens = _1 . glens @p
  {-# Inline glens #-}

instance GLens p f g => GLens ('GoRight p) (x :*: f) (x :*: g) where
  glens = _2 . glens @p
  {-# Inline glens #-}

instance GLens p f g => GLens p (S1 c f) (S1 d g) where
  glens = _M1 . glens @p
  {-# Inline glens #-}

instance GLens p (K1 i a) (K1 i b) where
  glens = _K1
  {-# Inline glens #-}




type family PathType p f where
  PathType p (M1 i c f) = PathType p f
  PathType ('GoLeft p) (f :*: g) = PathType p f
  PathType ('GoRight p) (f :*: g) = PathType p g
  PathType p (K1 i a) = a

data{-kind-} Path = Here | GoLeft Path | GoRight Path

type family MapMaybe (f :: a -> b) (m :: Maybe a) :: Maybe b where
  MapMaybe f ('Just x) = 'Just (f x)
  MapMaybe f 'Nothing  = 'Nothing

type family OrElse (m :: Maybe a) (n :: Maybe a) :: Maybe a where
  OrElse ('Just x) y = 'Just x
  OrElse 'Nothing  y = y

type family Find (s :: Symbol) (hay :: * -> *) :: Maybe Path where
  Find s (D1 c f) = Find s f
  Find s (C1 c f) = Find s f
  Find s (S1 ('MetaSel ('Just s) x y z) f) = 'Just 'Here
  Find s (x :*: y) = MapMaybe 'GoLeft (Find s x) `OrElse` MapMaybe 'GoRight (Find s y)
  Find s t = 'Nothing


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
  , GOptic n (Rep s) (Rep t) a b
  , GOpticCx n (Rep s) f
  , Functor f) =>
  LensLike f s t a b
genericOptic = generic' . goptic @n
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
lensA = cloneLens (genericOptic @"fieldA")

traversalB :: Traversal (Example1 a b) (Example1 a b') b b'
traversalB = boggling (genericOptic @"fieldB")

data HasPhantom a b = HasPhantom { hasPhantom :: b }
  deriving Generic

lensP :: Lens (HasPhantom a b) (HasPhantom a' b') b b'
lensP = genericOptic @"hasPhantom"
