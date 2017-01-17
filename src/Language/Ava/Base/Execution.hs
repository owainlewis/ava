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

-- All primative operations
data PrimOp = TPush AST.Value
            | TPop
            | TDup
            | TSwap
            | TCons
            | TUncons
            | TChoice
            | TStack
            | TUnstack
            -- | TIf
            -- | TIfte

std = M.fromList [ ("dup", dup)
                 ]

type App = Stack AST.Value -> ExceptT ProgramError IO (Stack AST.Value)

liftedOp :: Monad m => a -> ExceptT e m a
liftedOp = ExceptT . (return . Right)

eval :: PrimOp -> Stack AST.Value -> ExceptT ProgramError IO (Stack AST.Value)
eval TDup s = dup s
eval (TPush n) s = (push n) s

push :: Monad m => t -> Stack t -> ExceptT e m (Stack t)
push v s = liftedOp $ Stack.modify (\s -> v:s) s

dup :: App
dup s = liftedOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = (x:x:xs)

pop :: App
pop s = liftedOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = xs

swap :: Stack a -> Stack a
swap s = Stack.modify f s
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

execute :: (Foldable t, Monad m) => a -> t (a -> ExceptT e m a) -> m (Either e a)
execute s ops = runExceptT (foldM (\s f -> f s) s ops)
