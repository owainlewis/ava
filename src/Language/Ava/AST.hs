{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Ava.AST
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Defines the basic AST and core types for the Ava language
--
module Language.Ava.AST
    ( Value(..)
    , Instruction(..)
    , ProgramError(..)
    ) where

import Data.Semigroup((<>))

-- | -----------------------------------------------------------

data Value = Word String
           | Integer Int
           | Float Double
           | String String
           | Boolean Bool
           | List [Value]
           | Quotation [Value]
           | Procedure String [Value]
           deriving ( Show )

instance Eq Value where
  (Word x)            == (Word y)              = x == y
  (Integer x)         == (Integer y)           = x == y
  (Float x)           == (Float y)             = x == y
  (String x)          == (String y)            = x == y
  (Boolean x)         == (Boolean y)           = x == y
  (List xs)           == (List ys)             = xs == ys
  (Quotation xs)      == (Quotation ys)        = xs == ys
  (Procedure a xs)    == (Procedure b ys)    = xs == ys

-- | -----------------------------------------------------------

type Op    = String
type Arity = Int

-- | -----------------------------------------------------------

data ProgramError = InvalidArity Arity
                  | InvalidState Op
                  | GenericError String

instance Show ProgramError where
  show (InvalidArity n)  = "Invalid arity. Expecting {n} arguments"
  show (InvalidState op) = "Invalid state for operation " <> op
  show (GenericError e)  = e

-- | -----------------------------------------------------------

-- These types form the basic low operations for the Ava language.
--
-- An instruction can be thought of as a function that takes a stack as an input and returns either an error or a new stack.
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
                 | TDup
                 | TSwap
                 | TCons
                 | TUncons
                 | TChoice
                 | TStack
                 | TUnstack
                 | TLet
                 -- Numeric operations
                 | TMult
                 | TAdd
                 | TSub
                 | TDiv
                 -- Logical operations
                 | TGt
                 | TLt
                 | TEq
                 deriving ( Eq, Show )

-- | -----------------------------------------------------------
