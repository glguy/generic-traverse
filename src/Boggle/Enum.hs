{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Boggle.Enum (demos) where

import GHC.Generics
import Control.Monad
import Control.Applicative
import Control.Monad.Codensity
import Control.Monad.Trans.Class

class Enumerate a where
  enumerate :: [a]
  default enumerate :: (Generic a, GEnumerate (Rep a)) => [a]
  enumerate = lowerCodensity (to <$> genumerate)

class GEnumerate g where
  genumerate :: Codensity [] (g a)

instance GEnumerate f => GEnumerate (M1 i c f) where
  genumerate = M1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :+: g) where
  genumerate = L1 <$> genumerate
           <|> R1 <$> genumerate

instance (GEnumerate f, GEnumerate g) => GEnumerate (f :*: g) where
  genumerate = (:*:) <$> genumerate <*> genumerate

instance Enumerate b => GEnumerate (K1 i b) where
  genumerate = K1 <$> lift enumerate

instance GEnumerate U1 where
  genumerate = pure U1

instance GEnumerate V1 where
  genumerate = empty


data Demo = Z Bool | S Demo
  deriving (Show, Generic)

instance Enumerate Demo
instance Enumerate Bool

demos :: [Demo]
demos = enumerate
