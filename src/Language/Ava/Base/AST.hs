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
    , Op
    , Program
    ) where

import Data.Semigroup((<>))

-------------------------------------------------------------

type Op      = String
type Program = [Value]

-------------------------------------------------------------

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
           | Comment String
           deriving ( Ord, Show )

-------------------------------------------------------------

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
  (Comment c1)        == (Comment c2)          = c1 == c2

-------------------------------------------------------------

showType :: Value -> String
showType (Word x) = "Word"
showType (Apply x) = "Ap"
showType (Integer x) = "Integer"
showType (Float x) = "Float"
showType (String x) = "String"
showType (Boolean x) = "Boolean"
showType (List xs) = "List"
showType (Quotation xs) = "Quotation"
showType (Let k1 v1) = "Let"
showType (Define k1 v1) = "Define"
showType (Comment c1) = "Comment"
