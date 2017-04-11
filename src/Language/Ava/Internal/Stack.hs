module Language.Ava.Internal.Stack
  ( Stack(..)
  , empty
  , with
  , modify
  , modifyM
  , getStack
  , getVar
  , setVar
  , getProcedure
  , setProcedure
  ) where

import qualified Data.Map as M

-- | -----------------------------------------------------------
data Stack a = Stack
  { stack :: [a]
  , procedures :: M.Map String [a]
  , vars :: M.Map String a
  } deriving (Eq, Ord, Show)

-- | -----------------------------------------------------------
empty :: Stack a
empty = Stack [] M.empty M.empty

with :: [a] -> Stack a
with xs = Stack xs M.empty M.empty

-------------------------------------------------------------
modify :: ([a] -> [a]) -> Stack a -> Stack a
modify f (Stack s p v) = Stack (f s) p v

modifyM
  :: Monad m
  => ([a] -> m [a]) -> Stack a -> m (Stack a)
modifyM f (Stack s p v) = do
  state <- f s
  return (Stack state p v)

-------------------------------------------------------------
getStack :: Stack a -> [a]
getStack (Stack xs _ _) = xs

-------------------------------------------------------------
getVar :: String -> Stack a -> Maybe a
getVar k (Stack _ _ vs) = M.lookup k vs

setVar :: String -> a -> Stack a -> Stack a
setVar k v (Stack s ps vs) = Stack s ps (M.insert k v vs)

-------------------------------------------------------------
getProcedure :: String -> Stack a -> Maybe [a]
getProcedure k (Stack _ ps _) = M.lookup k ps

setProcedure :: String -> [a] -> Stack a -> Stack a
setProcedure k v (Stack s ps vs) = Stack s (M.insert k v ps) vs
