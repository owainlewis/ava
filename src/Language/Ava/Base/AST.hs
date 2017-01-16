{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Ava.Base.AST
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Defines the basic AST for the Ava language
--
module Language.Ava.Base.AST
    ( Value(..)
    ) where

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

data AvaError = GenericError String

instance Show AvaError where
  show (GenericError e) = e
