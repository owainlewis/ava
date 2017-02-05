-- |
-- Module      : Language.Ava.Core
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
module Language.Ava.Core
    ( execute
    , execute1
    ) where

import           Control.Monad.Except                  (ExceptT, foldM,
                                                        runExceptT)
import qualified Data.Text                             as T
import           Language.Ava.Base.AST
import           Language.Ava.Intermediate.Instruction
import qualified Language.Ava.Intermediate.Reader      as Reader
import           Language.Ava.Internal.Stack
import qualified Language.Ava.Internal.Stack           as Stack

import           Language.Ava.Apply                    (applyOp)

-- | Execute a sequence of intructions in the context of a given stack
--
execute :: Stack Value ->
           [Instruction] ->
           IO (Either ProgramError (Stack Value))
execute s ops = runExceptT (foldM (\s f -> applyOp f $ s) s ops)

-- | Takes a series of instructions and runs the on the empty stack
--
execute1 :: [Instruction] -> IO (Either ProgramError (Stack Value))
execute1 = execute Stack.empty

go :: T.Text -> IO (Either ProgramError (Stack Value))
go input = case Reader.readText input of
               Right instructions -> execute1 instructions
               Left parseErr -> return . Left $ GenericError (show parseErr)
