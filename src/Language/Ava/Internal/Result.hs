module Language.Ava.Internal.Result
    ( Result(..)
    , success
    , failure
    ) where

import Control.Applicative(liftA2)
import Data.Bifunctor

data Result e a = Failure e | Success a
    deriving ( Eq, Ord, Show, Read )

instance Functor (Result e) where
    fmap f (Success a) = Success (f a)
    fmap f (Failure e) = Failure e

instance Bifunctor Result where
    bimap f g = result (Failure . f) (Success . g)

instance Applicative (Result e) where
    pure = Success
    Failure e <*> _ = Failure e
    Success _ <*> Failure e = Failure e
    Success f <*> Success x = Success (f x)

instance Monad (Result e) where
    return = Success
    Success m >>= k = k m
    Failure e  >>= _ = Failure e

instance Foldable (Result a) where
    foldMap _ (Failure _) = mempty
    foldMap f (Success x) = f x

result :: (a -> c) -> (b -> c) -> Result a b -> c
result f g (Failure x) = (f x)
result f g (Success x) = (g x)

success :: t -> a -> Result e a
success x = Success

failure :: e -> Result e a
failure = Failure

bifmap :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
bifmap f = fmap . fmap $ f

fromEither :: Either e a -> Result e a
fromEither = either Failure Success

data ResultIO e a = ResultIO {
    runResultIO :: IO (Result e a)
}

instance Functor (ResultIO a) where
    fmap f = ResultIO . (bifmap f) . runResultIO

instance Applicative (ResultIO a) where
    pure    = ResultIO . return . Success
    f <*> x = ResultIO $ liftA2 (<*>) (runResultIO f) (runResultIO x)

instance Monad (ResultIO a) where
    return = pure
    x >>= f = ResultIO $ runResultIO x >>= g
                  where g = (\y -> case y of
                                       Success a -> runResultIO $ f a
                                       Failure  e -> return . Failure $ e)

