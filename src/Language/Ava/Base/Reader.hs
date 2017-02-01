{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Ava.Base.Reader
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Reads input strings and converts them into a concrete AST
--
module Language.Ava.Base.Reader
    ( readText
    , readString
    ) where

import Language.Ava.Base.Parser as P

import Language.Ava.Base.AST(Value)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readText :: T.Text -> Either AvaParseError [Value]
readText = P.parseMany

readString :: String -> Either AvaParseError [Value]
readString = readText . T.pack
