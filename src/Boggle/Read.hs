{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | This module demonstrates using 'BindK' to optimize a generic
-- implementation of the 'readsPrec' function. The immediate benefit
-- of doing the implementation within 'BindK' is that all of the uses
-- of 'fmap' in the implementation will be able to fuse into a single
-- pure function. Combined with inlining, this will gather the use of
-- 'to' with all of the generic representation constructors into the same
-- expression which GHC can then replace with the actual constructor.
--
-- This implementation does not parse record syntax.
module Boggle.Read
  ( -- * Generic 'readsPrec' implementation
    genericReadsPrec
    -- * 'ReadS' wrapper
  , Parse(..), lexP, readP, readParenP
    -- * Generic implementation classes
  , GRead(..), Fields(..)
    -- * Example uses
  , readUnit, readBool, readEither, readTriple, readMaybe
  ) where

import Control.Applicative      (Alternative(..), liftA2)
import Control.Monad            (MonadPlus, liftM, ap, guard)
import Data.Proxy               (Proxy(..))
import GHC.Generics

import Boggle                   (BindK(..), lowerBindK, liftBindK, liftBindK1)

------------------------------------------------------------------------

-- | 'Parse' wraps 'ReadS' in order to provide various typeclass instances.
newtype Parse a = MkParse { runParse :: ReadS a }

instance Functor Parse where
  fmap = liftM

instance Applicative Parse where
  pure x = MkParse (\s -> pure (x,s))
  (<*>)  = ap

instance Monad Parse where
  m >>= f = MkParse (\s -> do (x,s1) <- runParse m s; runParse (f x) s1)

instance Alternative Parse where
  empty   = MkParse (\_ -> empty)
  m <|> n = MkParse (\s -> runParse m s <|> runParse n s)

instance MonadPlus Parse

-- | Returns the next lexeme using 'lex'
lexP :: Parse String
lexP = MkParse lex

-- | Parse a value using 'readsPrec'
readP :: Read a => Int -> Parse a
readP = MkParse . readsPrec

-- | Wrap a parser to support nested parentheses. When the first argument
-- is 'True' surrounding parentheses are required, otherwise they are
-- optional.
readParenP :: Bool {- ^ parentheses required -} -> Parse a -> Parse a
readParenP b = MkParse . readParen b . runParse

------------------------------------------------------------------------

-- | Derived implementation of 'readsPrec' using generics.
genericReadsPrec :: (Generic a, GRead (Rep a)) => Int -> ReadS a
genericReadsPrec p = runParse (lowerBindK (to <$> greadsPrec p))

-- | Precedence of function application
funAppPrec :: Int
funAppPrec = 10

-- | Class for types that support generically derived 'readsPrec' functions.
--
-- The first argument is the precedence of the surrounding context.
--
-- This class uses the 'BindK' monad transfomer to reassociate all binds to
-- the right. This ensures that the use of the generics representation will
-- be able to collect into a single pure Haskell function enabling GHC to
-- optimize the generic representations away.
class GRead f where
  greadsPrec :: Int {- ^ precedence -} -> BindK Parse (f a)

-- | Data type metadata
instance GRead f => GRead (D1 c f) where
  greadsPrec p = M1 <$> greadsPrec p
  {-# INLINE greadsPrec #-}

-- | Multiple constructors
instance (GRead f, GRead g) => GRead (f :+: g) where
  greadsPrec p = L1 <$> greadsPrec p
             <|> R1 <$> greadsPrec p
  {-# INLINE greadsPrec #-}

-- | No constructors
instance GRead V1 where
  greadsPrec _ = empty
  {-# INLINE greadsPrec #-}

-- | One constructor
instance (Constructor c, Fields f) => GRead (C1 c f) where
  greadsPrec p = liftBindK1
                   (readParenP (fields && p > funAppPrec))
                   (M1 <$ parseConstructor <*> parseFields)
    where
      fields = hasFields (Proxy :: Proxy f)
      name   = conName (M1 Proxy :: C1 c Proxy ())

      parseConstructor =
        do str <- liftBindK lexP
           guard (str == name)
  {-# INLINE greadsPrec #-}

------------------------------------------------------------------------

-- | This class provides methods for parsing the fields of constructors
class Fields f where

  -- | Parse the fields in a generic representation. Leaf fields will be
  -- read at precedence 11 because they are always in the context of a
  -- constructor application.
  parseFields :: BindK Parse (f a)

  -- | Return 'True' if this generic structure has any fields at all.
  -- This is used by the constructor parser to decide when parentheses
  -- are necessary.
  hasFields :: proxy f -> Bool
  hasFields _ = True

-- | Field metadata
instance Fields f => Fields (S1 s f) where
  parseFields = M1 <$> parseFields
  {-# INLINE parseFields #-}

-- | Multiple fields
instance (Fields f, Fields g) => Fields (f :*: g) where
  parseFields = liftA2 (:*:) parseFields parseFields
  {-# INLINE parseFields #-}

-- | No fields
instance Fields U1 where
  parseFields = pure U1
  hasFields _ = False
  {-# INLINE parseFields #-}

-- | Single field
instance Read a => Fields (K1 i a) where
  parseFields = K1 <$> liftBindK (readP (funAppPrec + 1))
  {-# INLINE parseFields #-}

------------------------------------------------------------------------

-- | Derived implementation of 'readsPrec' for '()'
readUnit :: Int -> ReadS ()
readUnit = genericReadsPrec

-- | Derived implementation of 'readsPrec' for 'Bool'
readBool :: Int -> ReadS Bool
readBool = genericReadsPrec

readMaybe :: Read a => Int -> ReadS (Maybe a)
readMaybe = genericReadsPrec

-- | Derived implementation of 'readsPrec' for 'Either'
readEither :: (Read a, Read b) => Int -> ReadS (Either a b)
readEither = genericReadsPrec

-- | Derived implementation of 'readsPrec' for '(,,)'
readTriple :: (Read a, Read b, Read c) => Int -> ReadS (a,b,c)
readTriple = genericReadsPrec
