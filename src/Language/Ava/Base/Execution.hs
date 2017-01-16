module Language.Ava.Base.Execution
    (
    ) where

import           Control.Monad.Except
import           Language.Ava.Base.Stack    (Stack (..))

import qualified Language.Ava.Base.AST      as AST
import qualified Language.Ava.Base.Stack    as Stack
import qualified Data.Map as M

type Op    = String
type Arity = Int

data ProgramError = InvalidArity Arity
                  | InvalidState Op
                  | GenericError String

instance Show ProgramError where
  show (InvalidArity n)  = "Invalid arity. Expecting {n} arguments"
  show (InvalidState op) = "Invalid state for operation " ++ op
  show (GenericError e)  = e

std = M.fromList [ ("dup", dup)
                 ]

type Outcome a = ExceptT ProgramError IO (Stack a)

type App = Stack AST.Value -> ExceptT ProgramError IO (Stack AST.Value)

--push :: Stack a -> a -> Stack a
push :: Stack a -> a -> Stack a -> Stack a
push (Stack s y z) v = Stack.modify (\_ -> v:s)

dup :: App
dup s = ExceptT $ do
    liftIO (print "OK")
    return . Right $ Stack.modify f s
    where f []     = []
          f (x:xs) = (x:x:xs)

pop :: Stack a -> Stack a
pop = Stack.modify f
    where f []     = []
          f (x:xs) = xs

swap :: Stack a -> Stack a
swap = Stack.modify f
    where f []       = []
          f (x:y:xs) = y:x:xs
          f (x:xs)   = x:xs

cons :: Stack AST.Value -> Either ProgramError (Stack AST.Value)
cons = Stack.modifyM f
    where f (x : AST.Quotation xs : ys) = return $ (AST.Quotation (x : xs)) : ys
          f (x : AST.List xs : ys)      = return $ (AST.List (x : xs)) : ys
          f _                           = Left . InvalidState $ "cons"

uncons = error

choice = error

stack = error
unstack = error

-- tAKE A PROGRAM -> convert it to a sequence of operations
-- execute ech operation by aplying the op to stack
execute :: (Foldable t, Monad m) =>
           t (Stack a -> ExceptT e m (Stack a)) -> m (Either e (Stack a))
execute ops = runExceptT (foldM (\s f -> f s) (Stack.empty) ops)
