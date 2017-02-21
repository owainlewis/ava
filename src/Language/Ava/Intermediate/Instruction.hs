-- |
-- Module      : Language.Ava.Intermediate.Instruction
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
module Language.Ava.Intermediate.Instruction
    ( Instruction(..)
    ) where

import Language.Ava.Base.AST(Value, Op)

-- | -----------------------------------------------------------

-- These types form the basic low operations for the Ava language.
--
-- An instruction can be thought of as a function that takes a stack as
-- an input and returns either an error or a new stack.
--
-- There is really only one function type in this language.
--
-- @@ instruction :: Stack -k1 > IO ( Either ProgramError Stack ) @@
--
-- An operation may or may not perform IO.
--
-- Everything else gets re-written into this low level stack machine form
--
data Instruction = TPush Value
                 | TPop
                 | TApply Op
                 | TDefine Op [Value]
                 | TLet Op Value
                 | TDup
                 | TSwap
                 | TCons
                 | TUncons
                 | TChoice
                 | TStack
                 | TUnstack
                 | TInfra
                 | TMult
                 | TAdd
                 | TSub
                 | TDiv
                 | TGt
                 | TLt
                 | TEq
                 | TDot
                 | TPrint
                 | TNoop
                 deriving ( Eq, Show )
