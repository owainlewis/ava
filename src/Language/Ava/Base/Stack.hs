module Language.Ava.Base.Stack
    ( Stack(..)
    , empty
    , with
    , modify
    , modifyM
    , getVar
    , setVar
    , getProcedure
    , setProcedure
    ) where

import qualified Data.Map as M

data Stack a = Stack {
    -- The current runtime state
    stack      :: [a]
    -- A map of procedure names to a list of operations
  , procedures :: M.Map String [a]
    -- A map of variable names to a list of variable values
  , vars       :: M.Map String [a]
} deriving ( Eq, Ord, Show )

empty :: Stack a
empty = Stack [] M.empty M.empty

with :: [a] -> Stack a
with xs = Stack xs M.empty M.empty

modify :: ([a] -> [a]) -> Stack a -> Stack a
modify f (Stack s p v) = Stack (f s) p v

modifyM :: Monad m => ([a] -> m [a]) -> Stack a -> m (Stack a)
modifyM f (Stack s p v) = do
  state <- f s
  return (Stack state p v)

getVar :: Stack t -> String -> Maybe [t]
getVar (Stack _ _ vs) k = M.lookup k vs

setVar :: Stack a -> String -> [a] -> Stack a
setVar (Stack s ps vs) k v = Stack s ps (M.insert k v vs)

getProcedure :: String -> Stack t -> Maybe [t]
getProcedure k (Stack _ ps _) = M.lookup k ps

setProcedure :: Stack a -> String -> [a] -> Stack a
setProcedure (Stack s ps vs) k v = Stack s (M.insert k v ps) vs
