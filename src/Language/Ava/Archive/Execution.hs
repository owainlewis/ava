module Language.Ava.Execution
    ( execute
    , execute1
    ) where

import           Control.Monad.Except
import           Language.Ava.Internal.Stack (Stack (..))
import           Language.Ava.Apply(applyOp)


import qualified Language.Ava.AST   as AST
import qualified Language.Ava.Stack as Stack

import           Language.Ava.AST(Instruction(..), ProgramError(..))

-- | Execute a sequence of intructions in the context of a given stack
--
execute :: Foldable t =>
           Stack AST.Value ->
           t Instruction ->
           IO (Either ProgramError (Stack AST.Value))
execute s ops = runExceptT (foldM (\s f -> applyOp f $ s) s ops)

-- Executes a sequence of instructions over the empty stack
--
execute1
  :: (Foldable t, Monad m) =>
     t (Stack a -> ExceptT e m (Stack a)) ->
     m (Either e (Stack a))
execute1 ops = runExceptT (foldM (\s f -> f s) s ops)
                   where s = Stack.empty
