module Language.Ava.Core where

import           Language.Ava.Internal.Stack
import           Language.Ava.Base.AST
import           Language.Ava.Intermediate.Instruction

import           Control.Monad.Except(ExceptT, runExceptT, foldM)

import qualified Language.Ava.Internal.Stack as Stack

import Language.Ava.Apply(applyOp)

-- | Execute a sequence of intructions in the context of a given stack
--
execute :: Stack Value ->
           [Instruction] ->
           IO (Either ProgramError (Stack Value))
execute s ops = runExceptT (foldM (\s f -> applyOp f $ s) s ops)

-- Executes a sequence of instructions over the empty stack
--
execute1
  :: (Foldable t, Monad m) =>
     t (Stack a -> ExceptT e m (Stack a)) ->
     m (Either e (Stack a))
execute1 ops = runExceptT (foldM (\s f -> f s) s ops)
                   where s = Stack.empty



