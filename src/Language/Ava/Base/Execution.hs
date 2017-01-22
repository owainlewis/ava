module Language.Ava.Base.Execution
    ( PrimOp(..)
    , Op
    , Arity
    , execute
    , executeF
    ) where

import           Control.Monad.Except
import           Language.Ava.Base.Stack (Stack (..))

import qualified Data.Map                as M
import qualified Language.Ava.Base.AST   as AST
import qualified Language.Ava.Base.Stack as Stack

type Op    = String
type Arity = Int

-- | *********************************************************************

data ProgramError = InvalidArity Arity
                  | InvalidState Op
                  | GenericError String

instance Show ProgramError where
  show (InvalidArity n)  = "Invalid arity. Expecting {n} arguments"
  show (InvalidState op) = "Invalid state for operation " ++ op
  show (GenericError e)  = e

-- | *********************************************************************

type App = Stack AST.Value -> ExceptT ProgramError IO (Stack AST.Value)

-- | *********************************************************************

data PrimOp = TPush AST.Value
            | TPop
            | TDup
            | TSwap
            | TCons
            | TUncons
            | TChoice
            | TStack
            | TUnstack
            -- WIP
            | TLet
            deriving ( Eq, Show )

-- | *********************************************************************

fapply :: PrimOp -> Stack AST.Value -> ExceptT ProgramError IO (Stack AST.Value)
fapply (TPush v) s = push v s
fapply TPop s = pop s
fapply TDup s = dup s
fapply TSwap s = swap s
fapply TCons s = cons s
fapply TUncons s = uncons s
fapply TChoice s = choice s
fapply TLet s = letOp s

-- | *********************************************************************

liftOp :: Monad m => a -> ExceptT e m a
liftOp = ExceptT . (return . Right)

eval :: PrimOp -> Stack AST.Value -> ExceptT ProgramError IO (Stack AST.Value)
eval TDup s      = dup s
eval (TPush n) s = (push n) s

-- | *********************************************************************

push :: Monad m => t -> Stack t -> ExceptT e m (Stack t)
push v s = liftOp $ Stack.modify (\s -> v:s) s

pop :: App
pop s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = xs

dup :: App
dup s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = (x:x:xs)

swap :: App
swap s = liftOp $ Stack.modify f s
    where f (x:y:xs) = y:x:xs
          f x        = x

cons :: App
cons s = ExceptT . return $ Stack.modifyM f s
    where f (x : AST.Quotation xs : ys) =
              return $ (AST.Quotation (x : xs)) : ys
          f (x : AST.List xs : ys) =
            return $ (AST.List (x : xs)) : ys
          f _ = Left . InvalidState $ "cons"

uncons :: App
uncons s = ExceptT . return $ Stack.modifyM f s
    where f (AST.Quotation (x:xs) : ys) =
            return $ x : (AST.Quotation xs) : ys
          f (AST.List (x:xs) : ys) =
            return $ x : (AST.List xs) : ys
          f _ = Left . InvalidState $ "uncons"

choice :: App
choice s = ExceptT . return $ Stack.modifyM f s
    where f ((AST.Boolean b) : y : n : xs)  =
            if b then return $ y : xs
                 else return $ n : xs
          f _ = Left . InvalidState $ "choice"

--stack = error
--unstack = error

letOp :: App
letOp (Stack s p vs) = ExceptT . return $ do
    case s of
        ((AST.Word k) : v : xs) ->
            Right $ Stack xs p (M.insert k v vs)
        _ -> Left $ InvalidState "let"

-- | *********************************************************************

execute :: (Foldable t, Monad m) => a -> t (a -> ExceptT e m a) -> m (Either e a)
execute s ops = runExceptT (foldM (\s f -> f s) s ops)

executeF :: Foldable t => Stack AST.Value -> t PrimOp ->
            IO (Either ProgramError (Stack AST.Value))
executeF s ops = runExceptT (foldM (\s f -> fapply f $ s) s ops)
