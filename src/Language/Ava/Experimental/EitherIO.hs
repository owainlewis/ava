module Langage.Ava.Experimental.EitherIO
    ( bifmap
    , liftEither
    , liftIO
    , throwE
    ) where

import           Control.Applicative (liftA2)
import qualified Language.Ava.AST as AST
import qualified Data.Map as M

data Stack = Stack {
    runtime    :: [AST.Value]
  , procedures :: M.Map String [AST.Value]
  , vars       :: M.Map String (AST.Value)
} deriving ( Eq, Ord, Show )

data AvaError = StackArityInvalid Int

-- Defines the structure for a function that gets applied to the stack.
-- The information here is used fthe or type checking and arity checking
--
-- All native operations should be desugared into an application
--
-- Using the build in swap operation as an example:
--
-- The swap operation requires two items to be on the stack and both items can
-- be of any type
--
-- swap => App { arity = 2, name = "swap", typePattern = [AST.Any, AST.Any] }
--
--
data App = App { arity :: Int
               , typePattern :: [AST.TKind]
               }

swap :: App
swap = App { arity = 0, typePattern = [AST.TAny, AST.TAny] }

checkArity :: Applicative f => App -> Stack -> f (Result AvaError App)
checkArity ap@(App arity typePattern) (Stack runtime _ _) =
  let numberOfItemsOnStack = length runtime in
    if numberOfItemsOnStack < arity
    then pure . Failure $ StackArityInvalid arity
    else pure . Success $ ap

-- | **************************************************************
--
--  A type that represents the return value of running a single step
--
-- | **************************************************************
data Result a b = Failure a | Success b
    deriving (Eq, Ord, Read, Show)

instance Functor (Result a) where
    fmap f (Success x) = Success (f x)
    fmap f (Failure x) = Failure x

data ResultIO e a = ResultIO {
    runResultIO :: IO (Result e a)
}

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

--push :: Monad m => a -> [a] -> m (Either b [a])
--push v (Stack runtime p v) = return . Right $ 

--eval (AST.Integer x) (Stack runtime procs vars) =
--    return . Right $ Stack ()




