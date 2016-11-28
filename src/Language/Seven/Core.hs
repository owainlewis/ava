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

-- | Push an item onto the runtime stack
--
push :: Value -> VM ()
push x = runtime %= (x:)

-- | Pop an item off the stack
--
pop :: VM Value
pop = do
  stack <- get
  case stack of
    (Stack [] _) -> throwError StackUnderflowException
    s@(Stack (x:xs) _) -> do
          runtime .= xs
          return x

peek :: VM (Maybe Value)
peek = do
  stack <- get
  case stack of
    (Stack [] _) ->
        return Nothing
    (Stack (x:xs) _) ->
        return $ Just x

noop :: VM ()
noop = return ()

-- | Run a series of steps on the stack
--
-- λ> (run dot (Stack [Number 10] [])) >>= (\_ -> return ()):
--
run :: VM a -> Stack -> IO (Either ProgramError a, Stack)
run f s = runStateT (runExceptT (runVM f)) s

-- | Run a program but throw away the result
runIO :: Monad m => a -> b -> m ()
runIO f s = runIO f s >>= (\_ -> return ())

-- | Executes a program p (a list of operations to perform in sequential order)
--
eval :: Foldable a => a Value -> Stack -> IO (Either ProgramError (), Stack)
eval p stack = run (forM_ p eval) stack
    where eval (Number n) = push $ (Number n)
          eval (Word w) = symTab w

-- | Works like eval but doesn't require an initial input state
--
eval1 :: Foldable a => a Value -> IO (Either ProgramError (), Stack)
eval1 = flip eval $ Stack [] []

-- STD
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

dot :: VM ()
dot = do
  stack <- get
  liftIO . print . show $ stack

swap :: VM ()
swap = do
  x <- pop
  y <- pop
  push y
  push x

-- | ------------------------------------------------------------------------

symTab :: String -> VM ()
symTab "+" = binOp (+)
symTab _ = throwError $ RuntimeException "Unknown word"

