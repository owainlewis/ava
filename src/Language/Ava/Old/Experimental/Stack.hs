module Language.Ava.Experimental.Stack
    ( Stack
    ) where

import qualified Data.Map as M
-- import Control.Monad.Trans.Except
import           Control.Applicative (liftA2)

data Stack a = Stack {
    -- The current runtime state
    stack      :: [a]
    -- A map of procedure names to a list of operations
  , procedures :: M.Map String [a]
    -- A map of variable names to a list of variable values
  , vars       :: M.Map String [a]
} deriving ( Eq, Ord, Show )

emptyStack :: Stack a
emptyStack = Stack [] (M.empty) (M.empty)

modifyStack :: ([a] -> [a]) -> Stack a -> Stack a
modifyStack f (Stack s p v) = Stack (f s) p v

getVar :: Stack t -> String -> Maybe [t]
getVar (Stack _ _ vs) k = M.lookup k vs

setVar :: Stack a -> String -> [a] -> Stack a
setVar (Stack s ps vs) k v = Stack s ps (M.insert k v vs)

getProcedure :: String -> Stack t -> Maybe [t]
getProcedure k (Stack _ ps _) = M.lookup k ps

setProcedure :: Stack a -> String -> [a] -> Stack a
setProcedure (Stack s ps vs) k v = Stack s (M.insert k v ps) vs

-- | -------------------------------------------------------------------

data Result a b = Failure a | Success b
    deriving (Eq, Show)

instance Functor (Result a) where
    fmap f (Success x) = Success (f x)
    fmap f (Failure x) = Failure x

-- | -------------------------------------------------------------------

data ResultIO e a = ResultIO {
    runResultIO :: IO (Result e a)
}

instance Functor (ResultIO a) where
  fmap f = ResultIO . (fmap . fmap $ f) . runResultIO
