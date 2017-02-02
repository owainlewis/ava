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
    , readFile
    ) where

import Prelude hiding (readFile)

import Language.Ava.Base.Parser as P

import Language.Ava.Base.AST(Value)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type ParseOutcome = Either AvaParseError [Value]

readText :: T.Text -> ParseOutcome
readText = P.parseMany

readString :: String -> ParseOutcome
readString = readText . T.pack

readFile :: FilePath -> IO ParseOutcome
readFile path = readText <$> TIO.readFile path
