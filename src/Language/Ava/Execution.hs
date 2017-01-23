module Language.Ava.Execution
    ( execute
    , execute1
    ) where

import           Control.Monad.Except
import           Language.Ava.Stack (Stack (..))
import           Language.Ava.Apply(applyOp)


import qualified Language.Ava.AST   as AST
import qualified Language.Ava.Stack as Stack

import           Language.Ava.AST(PrimOp(..), ProgramError(..))


execute :: Foldable t => Stack AST.Value -> t PrimOp ->
            IO (Either ProgramError (Stack AST.Value))
execute s ops = runExceptT (foldM (\s f -> applyOp f $ s) s ops)

execute1 :: (Foldable t, Monad m) => a -> t (a -> ExceptT e m a) -> m (Either e a)
execute1 s ops = runExceptT (foldM (\s f -> f s) s ops)
