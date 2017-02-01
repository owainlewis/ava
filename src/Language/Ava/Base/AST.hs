{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Ava.Base.AST
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Defines the basic AST and core types for the Ava language
--
module Language.Ava.Base.AST
    ( Value(..)
    , ProgramError(..)
    , Op
    , Program
    ) where

import Data.Semigroup((<>))

type Op      = String

type Program = [Value]

data ProgramError =
                    InvalidState Op
                  | GenericError String

instance Show ProgramError where
  show (InvalidState op) = "Invalid state for operation " <> op
  show (GenericError e)  = e

-- | -----------------------------------------------------------
data Value = Word String
           | Apply String
           | Integer Int
           | Float Double
           | String String
           | Boolean Bool
           | List [Value]
           | Quotation [Value]
           | Let String Value
           | Define String [Value]
           deriving ( Show )

instance Eq Value where
  (Word x)            == (Word y)              = x == y
  (Apply x)           == (Apply y)             = x == y
  (Integer x)         == (Integer y)           = x == y
  (Float x)           == (Float y)             = x == y
  (String x)          == (String y)            = x == y
  (Boolean x)         == (Boolean y)           = x == y
  (List xs)           == (List ys)             = xs == ys
  (Quotation xs)      == (Quotation ys)        = xs == ys
  (Let k1 v1)         == (Let k2 v2)           = k1 == k2 && v1 == v2
  (Define k1 v1)      == (Define k2 v2)        = k1 == k2 && v1 == v2
