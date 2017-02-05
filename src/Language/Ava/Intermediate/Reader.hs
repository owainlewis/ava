-- |
-- Module      : Language.Ava.Intermediate.Reader
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
-- Stability   : experimental
-- Portability : GHC
--
module Language.Ava.Intermediate.Reader
    ( readText
    , readString
    , readFile
    , eval
    , loadAva
    , Outcome
    ) where

import           Prelude                               hiding (readFile)

import           Language.Ava.Base.Parser              (AvaParseError)
import           Language.Ava.Intermediate.Instruction (Instruction)

import qualified Language.Ava.Base.Reader              as Base

import           Language.Ava.Base.AST
import           Language.Ava.Intermediate.Instruction

import qualified Data.Text                             as T
import qualified Data.Text.IO                          as TIO

type Outcome = Either AvaParseError [Instruction]

-- | A phase1 transform operation from some value to a sequence of base
--   instructions
--
--   A value that returns `Nothing` is a value that cannot be interpreted
--
eval :: Value -> Instruction
eval (Integer x)   = (TPush (Integer x))
eval (Float x)     = (TPush (Float x))
eval (String x)    = (TPush (String x))
eval (List x)      = (TPush (List x))
eval (Quotation x) = (TPush (Quotation x))
eval (Boolean b)   = (TPush (Boolean b))
eval (Word w)      = (TApply w)
eval (Let k v)     = (TLet k v)
eval (Define k vs) = (TDefine k vs)

readText :: T.Text -> Outcome
readText s = fmap (map eval) (Base.readText s)

readString :: String -> Outcome
readString s = fmap (map eval) (Base.readString s)

readFile :: FilePath -> IO Outcome
readFile path = readText <$> TIO.readFile path

-- | Loads an AVA program from file
--
loadAva :: FilePath -> IO Outcome
loadAva filePath = readText <$> TIO.readFile filePath

