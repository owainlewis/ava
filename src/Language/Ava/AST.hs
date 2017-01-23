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
    , PrimOp(..)
    , ProgramError(..)
    ) where

-- | -----------------------------------------------------------

data Value = Word String
           | Integer Int
           | Float Double
           | String String
           | Boolean Bool
           | List [Value]
           | Quotation [Value]
           deriving ( Show )

instance Eq Value where
  (Word x)            == (Word y)              = x == y
  (Integer x)         == (Integer y)           = x == y
  (Float x)           == (Float y)             = x == y
  (String x)          == (String y)            = x == y
  (Boolean x)         == (Boolean y)           = x == y
  (List xs)           == (List ys)             = xs == ys
  (Quotation xs)      == (Quotation ys)        = xs == ys

-- | -----------------------------------------------------------

type Op    = String
type Arity = Int

-- | -----------------------------------------------------------

data ProgramError = InvalidArity Arity
                  | InvalidState Op
                  | GenericError String

instance Show ProgramError where
  show (InvalidArity n)  = "Invalid arity. Expecting {n} arguments"
  show (InvalidState op) = "Invalid state for operation " ++ op
  show (GenericError e)  = e

-- | -----------------------------------------------------------

data PrimOp = TPush Value
            | TPop
            | TDup
            | TSwap
            | TCons
            | TUncons
            | TChoice
            | TStack
            | TUnstack
            | TLet
            deriving ( Eq, Show )

-- | -----------------------------------------------------------
