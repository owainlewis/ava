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
import Data.Monoid((<>))

import qualified Data.Map as M

data Value = Word String
           | Number Int
           | Procedure String [Value]
  deriving ( Eq )

instance Show Value where
    show (Number n) = show n
    show (Word w) = show w
    show (Procedure k v) = "() " <> k <> " -> [...]"

data Stack = Stack {
    -- | The runtime stack that holds the current program state
    _runtime :: [Value]
    -- | The global environment used to storage user defined procs
  , _env :: M.Map String [Value]
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
    s@(Stack (x:xs) env) -> do
          runtime .= xs
          return x

-- | Check if a value exists. Do we need this?
--
peek :: VM (Maybe Value)
peek = do
    Stack rt _ <- get
    case rt of
        [] ->
            return Nothing
        (x:xs) ->
            return $ Just x

noop :: VM ()
noop = return ()

-- | Insert a sequence of operations into the current VM env
--
setEnv :: String -> [Value] -> VM ()
setEnv k v = env %= M.insert k v

-- | Extract a value from the environment
--
getEnv :: String -> VM (Maybe [Value])
getEnv k = do
    Stack _ env <- get
    return $ M.lookup k env

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

-- | Executes a program p (a list of operations to perform in sequential order)
--
eval :: Foldable a => a Value -> Stack -> IO (Either ProgramError (), Stack)
eval p stack = run (forM_ p eval) stack
    where eval (Number n) = push $ (Number n)
          eval (Word w) = do
                -- First we need to check in the current vm env to see if
                -- a user has defined the value of a word w to be some procdure p
                v <- getEnv w
                case v of
                    Just procedure -> noop
                    Nothing ->
                        -- If the value does not exist then we just try and execute it
                        symTab w
          eval (Procedure p instrs) = noop
              -- If there is a value found then we need to lookup the key
              -- and if it exists we append the result stack on to the
              -- input stack (?)

-- | Works like eval but doesn't require an initial input state
--
eval1 :: Foldable a => a Value -> IO (Either ProgramError (), Stack)
eval1 = flip eval $ Stack [] (M.empty)

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

debug :: VM ()
debug = get >>= liftIO . print . show

dot :: VM ()
dot = do
    Stack xs _ <- get
    case xs of
      (x:xs) -> liftIO . print . show $ x
      [] -> throwError $ RuntimeException "Empty stack"

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

