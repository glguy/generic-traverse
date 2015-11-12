{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Boggle.Read where

import Control.Applicative
import Control.Monad
import Data.Proxy
import GHC.Generics

import Boggle (BindK(..), lowerBindK, liftBindK, liftBindK1)

------------------------------------------------------------------------

newtype Parse a = Parse { runParse :: ReadS a }

instance Functor Parse where
  fmap = liftM

instance Applicative Parse where
  pure x = Parse (\s -> [(x,s)])
  (<*>) = ap

instance Monad Parse where
  m >>= f = Parse (\s -> do (x,s1) <- runParse m s; runParse (f x) s1)

instance Alternative Parse where
  empty = Parse (\_ -> empty)
  m <|> n = Parse (\s -> runParse m s <|> runParse n s)

instance MonadPlus Parse

lexP :: Parse String
lexP = Parse lex

readP :: Read a => Int -> Parse a
readP = Parse . readsPrec

readParenP :: Bool -> Parse a -> Parse a
readParenP b = Parse . readParen b . runParse

------------------------------------------------------------------------

genericReadsPrec :: (Generic a, GRead (Rep a)) => Int -> ReadS a
genericReadsPrec p = runParse (lowerBindK (to <$> greadsPrec p))

class GRead f where
  greadsPrec :: Int -> BindK Parse (f a)

-- | Used for 'D1' and 'S1'
instance GRead f => GRead (M1 i c f) where
  greadsPrec p = M1 <$> greadsPrec p

instance {-# OVERLAPPING #-}
  (Constructor c, GRead f, HasFields f) => GRead (C1 c f) where
  greadsPrec p
    = liftBindK1 (readParenP (p > 10 && hasFields (Proxy :: Proxy f)))
    $ do str <- liftBindK lexP
         guard (str == conName (M1 Proxy :: C1 c Proxy ()))
         M1 <$> greadsPrec 11
  {-# INLINE greadsPrec #-}

instance (GRead f, GRead g) => GRead (f :+: g) where
  greadsPrec p = L1 <$> greadsPrec p <|> R1 <$> greadsPrec p

instance (GRead f, GRead g) => GRead (f :*: g) where
  greadsPrec _ = liftA2 (:*:) (greadsPrec 11) (greadsPrec 11)

instance GRead U1 where
  greadsPrec _ = pure U1

instance GRead V1 where
  greadsPrec _ = empty

instance Read a => GRead (K1 i a) where
  greadsPrec p = K1 <$> liftBindK (readP p)

-- | This class helps determine when parentheses will be needed
class HasFields (f :: * -> *) where hasFields :: proxy f -> Bool
-- | No fields
instance HasFields U1 where hasFields _ = False
-- | Multiple fields
instance HasFields (f :*: g) where hasFields _ = True
-- | Single field
instance HasFields (M1 i c f) where hasFields _ = True

readunit :: Int -> ReadS ()
readunit = genericReadsPrec

readboolean :: Int -> ReadS Bool
readboolean = genericReadsPrec

reador :: (Read a, Read b) => Int -> ReadS (Either a b)
reador = genericReadsPrec

readthree :: (Read a, Read b, Read c) => Int -> ReadS (a,b,c)
readthree = genericReadsPrec
