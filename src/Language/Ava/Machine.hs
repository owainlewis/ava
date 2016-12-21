{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |
-- Module      : Language.Ava.Machine
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Language.Ava.Machine where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Language.Ava.AST

import qualified Data.Map             as M

data Stack = Stack {
    -- The runtime stack that holds the current program state
    _runtime    :: [Value]
    -- Defined procedures
  , _procedures :: M.Map String [Value]
    -- User defined let bindings
  , _vars       :: M.Map String Value
} deriving ( Eq, Show )

makeLenses ''Stack

newtype VM a = VM { runVM :: ExceptT ProgramError (StateT Stack IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState Stack
             , MonadError ProgramError
             , MonadIO
             )

data ProgramError =
      RuntimeException String
    | TypeError String

instance Show ProgramError where
  show (RuntimeException e) = e
  show (TypeError e)        = e

raise :: ProgramError -> VM ()
raise err =
    let printio = liftIO . print in
    do
        printio "=== Exception ==="
        printio $ show err
        throwError err

withArity :: Int -> VM ()
withArity n = do
    xs <- use runtime
    if (length xs < n)
      then raise $ RuntimeException (concat [
              "Expecting at least "
            , show n
            , " elements on the stack but found only "
            , show $ length xs
            ])
      else return ()

push :: Value -> VM ()
push x = runtime %= (x:)

pop :: VM Value
pop = use runtime >>= \case
      []     -> throwError $ RuntimeException "Stack underflow"
      (x:xs) -> runtime .= xs >> return x

noop :: VM ()
noop = return ()

-- | Insert a sequence of operations into the current VM procedures map
--
setProcedure :: String -> [Value] -> VM ()
setProcedure k v = procedures %= M.insert k v

-- | Extract a procedure from the defined procedures map
--
getProcedure :: String -> VM (Maybe [Value])
getProcedure k = M.lookup k <$> use procedures

-- | Insert a sequence of operations into the current VM procedures map
--
setVar :: String -> Value -> VM ()
setVar k v = vars %= M.insert k v

-- | Extract a procedure from the defined procedures map
--
getVar :: String -> VM (Maybe Value)
getVar k = M.lookup k <$> use vars

-- Fetch the runtime state
--
getRuntime :: VM [Value]
getRuntime = use runtime

-- Set the runtime state to some constant value
--
setRuntime :: [Value] -> VM ()
setRuntime xs = runtime .= xs

-- | Apply a function to modify the current runtime
--
modifyRuntime :: ([Value] -> [Value]) -> VM ()
modifyRuntime f = setRuntime . f =<< getRuntime

-- | Run a series of steps on the stack
--
-- λ> (run debug (Stack [Number 10] M.empty)) >>= (\_ -> return ())
--
run :: VM a -> Stack -> IO (Either ProgramError a, Stack)
run f s = runStateT (runExceptT (runVM f)) s

-- | Run a program but throw away the result
--
runIO :: Monad m => a -> b -> m ()
runIO f s = runIO f s >>= (\_ -> return ())
