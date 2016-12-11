{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Seven.Core
  ( VM(..)
  , Stack(..)
  , ProgramError(..)
  -- Stack operations
  , raise
  , push
  , pop
  , peek
  , noop
  -- Environment
  , getEnv
  , setEnv
  -- Execution
  , run
  , runIO
  -- Evaluation
  , eval
  , evalS
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Maybe               (fromMaybe)

import           Data.Monoid              ((<>))
import           Language.Seven.AST
import           Language.Seven.Parser    (parseSeven)

import qualified Data.Map                 as M

data Stack = Stack {
    -- | The runtime stack that holds the current program state
    _runtime :: [Value]
    -- | The global environment used to storage user defined procedures
  , _env     :: M.Map String [Value]
} deriving ( Eq )

instance Show Stack where
  show (Stack xs _) = show xs

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
    | TypeError String

instance Show ProgramError where
  show StackUnderflowException = "Stack underflow"
  show (RuntimeException e)    = e
  show (TypeError e) = e
  -- show (TypeError actual expected) = concat ["TypeError: Found: "
  --                                           , actual
  --                                           , "expecting"
  --                                           , expected
  --                                           ]

raise :: ProgramError -> VM ()
raise err = do
  liftIO (print "*** Runtime Error: Seven ***")
  liftIO (print . show $ err)
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
      []     -> throwError StackUnderflowException
      (x:xs) -> runtime .= xs >> return x

peek :: VM (Maybe Value)
peek = do
    Stack rt _ <- get
    case rt of
        []     -> return Nothing
        (x:xs) -> return $ Just x

noop :: VM ()
noop = return ()

-- | Insert a sequence of operations into the current VM env
--Ce
setEnv :: String -> [Value] -> VM ()
setEnv k v = env %= M.insert k v

-- | Extract a value from the environment
--
getEnv :: String -> VM (Maybe [Value])
getEnv k = M.lookup k <$> use env

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
-- eval1 [Procedure ">" [Number 20, Number 20, Word "+"], Word ">"]
evalS :: [Value] -> Stack -> IO (Either ProgramError (), Stack)
evalS p stack = run (forM_ p evaluate) stack
    where evaluate (Number n) = push $ Number n
          evaluate (FList xs) = push $ FList xs
          evaluate (Comment _) = noop
          evaluate (Word w) = do
                -- First we need to check in the current vm env to see if
                -- a user has defined the value of a word w to be some procedure p
                v <- getEnv w
                case v of
                    -- If the value exists then evaluate the procedure
                    Just procedure -> mapM_ evaluate procedure
                    -- Else lookup in the symbol table
                    Nothing ->
                        case (M.lookup w symTab) of
                            Just procedure -> procedure
                            Nothing -> raise $ RuntimeException ("Unbound word " <> w)
          -- | Evaluate a procedure by updating the environment
          evaluate (Procedure p instrs) = setEnv p instrs

-- | Works like eval but doesn't require an initial input state
--
eval :: [Value] -> IO (Either ProgramError (), Stack)

eval = flip evalS $ Stack [] (M.empty)
-- | Standard Library
-- ----------------------------------------------------------

symTab :: M.Map String (VM ())
symTab = M.fromList [ ("+", binOp (+))
                    , ("-", binOp (-))
                    , ("*", binOp (*))
                    , ("print", printTop)
                    , ("swap", swap)
                    , ("debug", debug)
                    , ("inc", increment)
                    , ("kill", kill)
                    , ("xkill", xkill)
                    , ("cons", fcons)
                    ]

-- | Apply a binary operation to two elements on the stack
--
binOp :: (Int -> Int -> Int) -> VM ()
binOp op = do
  withArity 2
  x <- pop
  y <- pop
  case (x,y) of
    (Number x1, Number y1) -> push $ Number (x1 `op` y1)
    _ -> raise $ RuntimeException "Expecting two integers"

printTop :: VM ()
printTop = do
    xs <- use runtime
    case xs of
      (x:xs) -> liftIO $ print x
      []     -> liftIO $ print "()"

swap :: VM ()
swap = do
  withArity 2
  x <- pop
  y <- pop
  push x
  push y

debug :: VM ()
debug = do
  stack <- get
  liftIO (putStrLn . show $ stack)

kill :: VM ()
kill = do
    xs <- use runtime
    case xs of
      (x:xs) -> runtime .= xs >> return ()
      []     -> return ()

xkill :: VM ()
xkill = do
    xs <- use runtime
    case xs of
        ((Number n):xs) -> runtime .= drop n xs >> return ()
        _               -> raise $ TypeError "Expecting integer"

increment :: VM ()
increment = do
    xs <- use runtime
    case xs of
      ((Number n):xs) -> runtime .= ((Number $ n+1):xs)
      _ -> raise $ TypeError "Expecting numeric argument"

fcons :: VM ()
fcons = do
  xs <- use runtime
  case xs of
      (x:(FList ys):xs) -> runtime .= (FList $ x : ys):xs
      _ -> raise $ TypeError "Cons requires a list"

-- List operations

