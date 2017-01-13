module Langage.Ava.Experimental.EitherIO
    ( bifmap
    , liftEither
    , liftIO
    , throwE
    ) where

import           Control.Applicative (liftA2)

data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

-- How do we derive show without unwrapping IO ?

bifmap :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
bifmap f = fmap . fmap $ f

instance Functor (EitherIO a) where
    fmap f = EitherIO . (bifmap f) . runEitherIO

instance Applicative (EitherIO a) where
    pure = EitherIO . return . Right
    f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO a) where
    return = pure
    x >>= f = EitherIO $ runEitherIO x >>= g
                  where g = (\y -> case y of
                                       Right a -> runEitherIO $ f a
                                       Left  e -> return . Left $ e)

liftEither :: Either e a -> EitherIO e a
liftEither x = EitherIO $ pure x

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)

throwE :: e -> EitherIO e a
throwE x = liftEither (Left x)

catchE :: EitherIO t (Either e a) -> (t -> EitherIO e a) -> EitherIO e a
catchE throwing handler = EitherIO $ do
    result <- runEitherIO throwing
    case result of
       Left e -> runEitherIO (handler e)
       Right a -> return a

data ProgramError = Fail


