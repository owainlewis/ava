module Language.Ava.Internal.Result
  ( Result(..)
  , success
  , failure
  , result
  , fromEither
  ) where

import Data.Bifunctor (bimap, Bifunctor)

-----------------------------------------------------------
data Result e a
  = Failure e
  | Success a
  deriving (Eq, Ord, Show, Read)

-----------------------------------------------------------
instance Functor (Result e) where
  fmap f (Success a) = Success (f a)
  fmap f (Failure e) = Failure e
  {-# INLINE fmap #-}

-----------------------------------------------------------
instance Bifunctor Result where
  bimap f g = result (Failure . f) (Success . g)
  {-# INLINE bimap #-}

-----------------------------------------------------------
instance Applicative (Result e) where
  pure = Success
  {-# INLINE pure #-}
  Failure e <*> _ = Failure e
  Success _ <*> Failure e = Failure e
  Success f <*> Success x = Success (f x)
  {-# INLINE <*> #-}

-----------------------------------------------------------
instance Monad (Result e) where
  return = Success
  Success m >>= k = k m
  Failure e >>= _ = Failure e
  {-# INLINE >>= #-}

-----------------------------------------------------------
instance Foldable (Result a) where
  foldMap _ (Failure _) = mempty
  foldMap f (Success x) = f x

-----------------------------------------------------------
result :: (a -> c) -> (b -> c) -> Result a b -> c
result f g (Failure x) = (f x)
result f g (Success x) = (g x)

success :: t -> a -> Result e a
success x = Success

failure :: e -> Result e a
failure = Failure

fromEither :: Either e a -> Result e a
fromEither = either Failure Success
