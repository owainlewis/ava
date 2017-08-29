module Language.Ava.Internal.ResultIO
  ( ResultIO
  , liftSuccessIO
  , liftFailureIO
  ) where

import Control.Applicative (liftA2)

import Language.Ava.Internal.Result

bifmap
  :: (Functor f, Functor g)
  => (a -> b) -> g (f a) -> g (f b)
bifmap = fmap . fmap

newtype ResultIO e a = ResultIO { runResultIO :: IO (Result e a) }

instance Functor (ResultIO a) where
  fmap f = ResultIO . (bifmap f) . runResultIO
  {-# INLINE fmap #-}

instance Applicative (ResultIO a) where
  pure = ResultIO . return . Success
  {-# INLINE pure #-}
  f <*> x = ResultIO $ liftA2 (<*>) (runResultIO f) (runResultIO x)
  {-# INLINE <*> #-}

instance Monad (ResultIO a) where
  return = pure
  x >>= f = ResultIO $ runResultIO x >>= g
    where
      g =
        (\y ->
           case y of
             Success a -> runResultIO $ f a
             Failure e -> return . Failure $ e)
  {-# INLINE >>= #-}
  
liftSuccessIO :: a -> ResultIO e a
liftSuccessIO = ResultIO . return . Success

liftFailureIO :: e -> ResultIO e a
liftFailureIO = ResultIO . return . Failure
