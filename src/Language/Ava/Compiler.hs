-- |
-- Module      : Language.Ava.Compiler
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
module Language.Ava.Compiler
  ( compile
  , execute
  , executeWithStdLib
  , go
  , goWithStack
  ) where

import Control.Monad.Except (ExceptT, foldM, runExceptT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Language.Ava.Base.AST
import Language.Ava.Base.Error
import Language.Ava.Instruction
import Language.Ava.Internal.Stack
import qualified Language.Ava.Internal.Stack as Stack
import qualified Language.Ava.Reader as Reader

import Language.Ava.Apply (applyOp)

import Data.Bifunctor (bimap)

data CompileOptions =
  LoadStd Bool

-- | Execute a sequence of intructions in the context of a given stack
--
execute :: Stack Value
        -> [Instruction]
        -> IO (Either ProgramError (Stack Value))
execute ss ops = runExceptT $ foldM (\s f -> applyOp f s) ss ops

----------------------------------------------------------------
-- Load the standard library
-- Runs a set of instructions over a stack that has the standard library preloaded
--
executeWithStdLib :: [Instruction] -> IO (Either ProgramError (Stack Value))
executeWithStdLib ops = do
  result <- go =<< TIO.readFile "lib/language.ava"
  either (return . Left) (flip execute ops) result

---------------------------------------------------------------
goWithStack :: Stack Value -> T.Text -> IO (Either ProgramError (Stack Value))
goWithStack stack input =
  either
    (return . Left . GenericError . show)
    (print >> execute stack)
    (Reader.readText input)

go :: T.Text -> IO (Either ProgramError (Stack Value))
go input = goWithStack (Stack.empty) input

compile :: T.Text -> IO (Either ProgramError (Stack Value))
compile input =
  either
    (return . Left . GenericError . show)
    (print >> executeWithStdLib)
    (Reader.readText input)
