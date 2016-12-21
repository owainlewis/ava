-- |
-- Module      : Language.Ava.Eval
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Language.Ava.Eval
    ( evalS
    , eval
    ) where

import           Control.Exception     hiding (TypeError, evaluate)
import           Control.Monad         (forM_)
import           Control.Monad.State
import           Language.Ava.AST
import           Language.Ava.Machine

import qualified Language.Ava.Std.Base as Std

import qualified Data.Map              as M

-- | Executes a program p (a list of operations to perform in sequential order)
--
evalS :: [Value] -> Stack -> IO (Either ProgramError (), Stack)
evalS p stack = run (forM_ p evaluate) stack

evaluate :: Value -> VM ()
evaluate (Integer n) = push $ Integer n
evaluate (Float n)   = push $ Float n
evaluate (Vector xs) = push $ Vector xs
evaluate (Boolean b) = push $ Boolean b
evaluate (String s)  = push $ String s
evaluate (IfStmt cond pos neg) = do
    outcome <- mapM_ evaluate cond
    runtime <- getRuntime
    case runtime of
      (Boolean b:xs) -> do
        setRuntime xs
        if b then mapM_ evaluate pos
             else mapM_ evaluate neg
      _ ->
          noop
evaluate (Word w) = do
    -- First we need to check in the current vm env to see if
    -- a user has defined the value of a word w to be some procedure p
    v <- getEnv w
    case v of
        -- If the value exists then evaluate the procedure
        Just procedure -> mapM_ evaluate procedure
        -- Else lookup in the symbol table
        Nothing ->
            case (M.lookup w Std.symTab) of
                Just procedure ->
                    procedure
                Nothing ->
                    raise $ RuntimeException ("Unbound word " ++ w)
-- | Evaluate a procedure by updating the environment
evaluate (Procedure p instrs) = setEnv p instrs

-- | Works like eval but doesn't require an initial input state
--
eval :: [Value] -> IO (Either ProgramError (), Stack)
eval = flip evalS $ Stack [] (M.empty)
