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
    ( Op
    , Program
    , Prim(..)
    , Value(..)
    ) where

import qualified Data.Monoid as Monoid

import Data.List(intersperse)

-------------------------------------------------------------

type Op      = String
type Program = [Value]

listify xs = let f = foldl1 (Monoid.<>) . (intersperse ",") . map show in
             if null xs then Monoid.mempty
                        else f xs

-------------------------------------------------------------
-- Revised AST
-------------------------------------------------------------

data Prim = Word String
         | Integer Integer
         | Double Double
         | String String
         | Boolean Bool
         | List [Prim]
         | Quotation [Prim]
         | Apply Op
         deriving ( Eq )

wrapped :: Show a => String -> [a] -> String -> String
wrapped inner form outer = unwords [inner, listify form, outer]

instance Show Prim where
    show (Word x) = mconcat ["Word", show x]
    show (Integer x) = show x
    show (Double x) = show x
    show (String x) = show x
    show (Boolean x) = show x
    show (Apply f) = mconcat ["Lambda", show f]
    show (List xs) = wrapped "[" xs "]"
    show (Quotation xs) = wrapped "{" xs "}"

data Value = Prim Prim
           | Let String Value
           | Define String [Value]
           | Comment String
           | Import String (Maybe String)
           deriving ( Eq )

instance Show Value where
    show (Prim p) = show p
    show (Let k v) = unwords ["Let", show k, "=", show v]
    show (Define k vs) = unwords ["Define", k, "[", "]"]
