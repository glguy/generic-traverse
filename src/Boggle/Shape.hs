{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
module Boggle.Shape where

data Shape :: (* -> *) -> * -> * where
  Map :: (a -> b) -> Shape f a -> Shape f b
  Ap  :: Shape f (a -> b) -> Shape f a -> Shape f b
  Pure :: a -> Shape f a
  Lift :: f a -> Shape f a

instance Functor (Shape f) where
  fmap = Map

instance Applicative (Shape f) where
  (<*>) = Ap
  pure  = Pure

liftShape :: f a -> Shape f a
liftShape = Lift

lowerShape :: Applicative f => Shape f a -> f a
lowerShape (Map f x) = fmap f (lowerShape x)
lowerShape (Ap f x) = lowerShape f <*> lowerShape x
lowerShape (Pure x) = pure x
lowerShape (Lift x) = x

showShape :: Shape f a -> String
showShape s = showShapePrec 0 s ""

showShapePrec :: Int -> Shape f a -> ShowS
showShapePrec _ Lift{} = showString "Lift _"
showShapePrec _ Pure{} = showString "Pure _"
showShapePrec p (Map _ x)
  = showParen (p > 4)
  $ showString "_ <$> "
  . showShapePrec 5 x
showShapePrec p (Ap f x)
  = showParen (p > 4)
  $ showShapePrec 4 f
  . showString " <*> "
  . showShapePrec 5 x
