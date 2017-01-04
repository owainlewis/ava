-- |
-- Module      : Language.Ava.Eval
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimenotal
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

import qualified Language.Ava.Core as Core

import qualified Data.Map              as M

-- | Executes a program p (a list of operations to perform in sequential order)
--
evalS :: [Value] -> Stack -> IO (Either ProgramError (), Stack)
evalS p stack = run (forM_ p evaluate) stack

evaluate :: Value -> VM ()
evaluate (Integer n)           = push $ Integer n
evaluate (Float n)             = push $ Float n
evaluate (List xs)             = push $ List xs
evaluate (Boolean b)           = push $ Boolean b
evaluate (String s)            = push $ String s
evaluate (Quotation xs)        = push $ Quotation xs
evaluate (LetStmt k v)         = setVar k v
evaluate (IfStmt cond pos neg) =
    do
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
    p <- getProcedure w
    case p of
        -- If the value exists then evaluate the procedure
        Just procedure -> mapM_ evaluate procedure
        -- Else lookup in the symbol table
        Nothing ->
            case (M.lookup w Core.language) of
                Just procedure ->
                    procedure
                Nothing -> do
                    v <- getVar w
                    case v of
                      Just value -> evaluate value
                      Nothing ->
                          raise $ RuntimeException ("Unbound word " ++ w)
-- | Evaluate a procedure by updating the environment
evaluate (Procedure p instrs _) = setProcedure p instrs

-- | Works like eval but doesn't require an initial input state
--
eval :: [Value] -> IO (Either ProgramError (), Stack)
eval = flip evalS $ Stack [] (M.empty) (M.empty)
