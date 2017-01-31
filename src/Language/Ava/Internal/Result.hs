module Language.Ava.Internal.Result
    ( Result(..)
    , success
    , failure
    ) where

data Result e a = Failure e | Success a

instance Functor (Result e) where
    fmap f (Success a) = Success (f a)
    fmap f (Failure e) = Failure e

instance Applicative (Result e) where
    pure = Success
    Failure e <*> _ = Failure e
    Success _ <*> Failure e = Failure e
    Success f <*> Success x = Success (f x)

instance Monad (Result e) where
    return = Success
    Success m >>= k = k m
    Failure e  >>= _ = Failure e

success :: t -> a -> Result e a
success x = Success

failure :: e -> Result e a
failure = Failure

