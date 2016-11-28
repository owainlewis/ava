{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Seven.Core
    ( run
    , push
    , pop
    , peek
    ) where

import Control.Monad.State
import Control.Monad.Except
import Control.Lens

import qualified Data.Map as M

data Value = Word String
           | Number Int
           | Procedure String [Value]
  deriving ( Eq, Show )

data Stack = Stack {
    -- | The runtime stack that holds the current program state
    _runtime :: [Value]
    -- | The global environment used to storage user defined procs
  , _env :: [(String, [Value])]
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
      StackUnderflowException
    | RuntimeException String

instance Show ProgramError where
  show StackUnderflowException = "Error: Stack underflow"
  show (RuntimeException e) = e

environment :: [(String, VM ())]
environment = [ ("+", binOp (+))
              ]


-- | Push an item onto the runtime stack
--
--   liftIO . print $  "PUSH"
push :: Value -> VM ()
push x = do
    modify f
    where f (Stack output e) = Stack (x:output) e

-- | Pop an item off the stack
--
pop :: VM Value
pop = do
  stack <- get
  case stack of
    (Stack [] _) -> throwError StackUnderflowException
    s@(Stack (x:xs) _) -> do
          put $ s { _runtime = xs }
          return x

peek :: VM (Maybe Value)
peek = do
  stack <- get
  case stack of
    (Stack [] _) ->
        return Nothing
    (Stack (x:xs) _) ->
        return . pure $ x

noop :: VM ()
noop = return ()

-- | ------------------------------------------------------------------------

-- | Apply a binary operation to two elements on the stack
--
binOp :: (Int -> Int -> Int) -> VM ()
binOp op = do
  x <- pop
  y <- pop
  case (x,y) of
    (Number x1, Number y1) -> push $ Number (x1 `op` y1)
    _ -> throwError $ RuntimeException "Expecting two integers"

-- | ------------------------------------------------------------------------

symTab "+" = binOp (+)
symTab _ = throwError $ RuntimeException "Unknown word"

-- | Run a series of steps on the stack
--
run :: VM a -> Stack -> IO (Either ProgramError a, Stack)
run f s = runStateT (runExceptT (runVM f)) s

exec :: Foldable a => a Value -> Stack -> IO (Either ProgramError (), Stack)
exec program stack = run (forM_ program eval) stack
    where eval (Number n) = push $ (Number n)
          eval (Word w) = symTab w


