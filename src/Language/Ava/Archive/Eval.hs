-- |
-- Module      : Language.Ava.Eval
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
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

import           Data.Semigroup((<>))
import           Data.Maybe(maybe)

import qualified Language.Ava.Stack as S
import qualified Data.Map as M

liftO :: Applicative f => a -> f [a]
liftO = pure . pure

-- | A phase1 transform operation from some value to a sequence of base
--   instructions
--
--   A value that returns `Nothing` is a value that cannot be interpreted
--
eval :: Value -> Either ProgramError [Instruction]
eval (Integer x)      = liftO (TPush (Integer x))
eval (Float x)        = liftO (TPush (Float x))
eval (String x)       = liftO (TPush (String x))
eval (List x)         = liftO (TPush (List x))
eval (Quotation x)    = liftO (TPush (Quotation x))
eval (Boolean b)      = liftO (TPush (Boolean b))
eval (Word w)         = liftO (TApply w)
eval (Let k v)        = liftO (TLet k v)
eval (Define k vs)    = liftO (TDefine k vs)
