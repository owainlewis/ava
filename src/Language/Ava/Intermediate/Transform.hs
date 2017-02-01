module Language.Ava.Intermediate.Transform where

import           Language.Ava.Internal.Stack
import           Language.Ava.Base.AST
import           Language.Ava.Intermediate.Instruction

import           Control.Monad.Except(ExceptT, runExceptT, foldM)

import qualified Language.Ava.Internal.Stack as Stack

liftO :: Applicative f => a -> f [a]
liftO = pure . pure

-- | A phase1 transform operation from some value to a sequence of base
--   instructions
--
--   A value that returns `Nothing` is a value that cannot be interpreted
--
eval :: Value -> Instruction
eval (Integer x)      = (TPush (Integer x))
eval (Float x)        = (TPush (Float x))
eval (String x)       = (TPush (String x))
eval (List x)         = (TPush (List x))
eval (Quotation x)    = (TPush (Quotation x))
eval (Boolean b)      = (TPush (Boolean b))
eval (Word w)         = (TApply w)
eval (Let k v)        = (TLet k v)
eval (Define k vs)    = (TDefine k vs)

-- -- | Execute a sequence of intructions in the context of a given stack
-- --
-- execute :: Foldable t =>
--            Stack Value ->
--            t Instruction ->
--            IO (Either ProgramError (Stack Value))
-- execute s ops = runExceptT (foldM (\s f -> applyOp f $ s) s ops)

-- -- Executes a sequence of instructions over the empty stack
-- --
-- execute1
--   :: (Foldable t, Monad m) =>
--      t (Stack a -> ExceptT e m (Stack a)) ->
--      m (Either e (Stack a))
-- execute1 ops = runExceptT (foldM (\s f -> f s) s ops)
--                    where s = Stack.empty

