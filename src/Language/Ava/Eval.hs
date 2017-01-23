-- |
-- Module      : Language.Ava.Eval
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Evaluates a input from the parser and returns an operation / instruction
--
module Language.Ava.Eval
    ( eval )
    where

import           Language.Ava.AST
import           Language.Ava.Execution
import           Data.Maybe(maybe)

import qualified Data.Map as M

-- | A transform operation from Value to PrimOp
--
--   A Value that returns `Nothing` is a Value that cannot be interpreted
--
eval :: Value -> Either ProgramError PrimOp
eval (Integer x)     = Right (TPush (Integer x))
eval (Float x)       = Right (TPush (Float x))
eval (String x)      = Right (TPush (String x))
eval (List x)        = Right (TPush (List x))
eval (Quotation x)   = Right (TPush (Quotation x))
eval (Boolean b)     = Right (TPush (Boolean b))
eval (Word w)        = maybe (Left $ GenericError "Unbound word")
                             (\op -> Right op)
                             (M.lookup w allWords)
    where allWords = M.fromList [ ("dup", TDup)
                                , ("cons", TCons)
                                , ("uncons", TUncons)
                                , ("pop", TPop)
                                , ("choice", TChoice)
                                , ("let", TLet)
                                ]
