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
module Language.Ava.Reader
    ( readText
    , readString
    , readFile
    , eval
    , loadAva
    , Outcome
    ) where

import           Prelude                               hiding (readFile)

import           Language.Ava.Base.Parser              (AvaParseError)

import qualified Language.Ava.Base.Reader              as Base

import           Language.Ava.Base.AST
import           Language.Ava.Instruction

import qualified Data.Text                             as T
import qualified Data.Text.IO                          as TIO

type Outcome = Either AvaParseError [Instruction]

-- | A phase1 transform operation from some value to a sequence of base
--   instructions
--
--   A value that returns `Nothing` is a value that cannot be interpreted
--
eval :: Value -> Instruction
eval (Prim (Integer x))   = (TPush (Prim (Integer x)))
eval (Prim (Double x))    = (TPush (Prim (Double x)))
eval (Prim (String x))    = (TPush (Prim (String x)))
eval (Prim (List x))      = (TPush (Prim (List x)))
eval (Prim (Quotation x)) = (TPush (Prim (Quotation x)))
eval (Prim (Boolean b))   = (TPush (Prim (Boolean b)))
eval (Prim (Word w))      = (TApply w)
eval (Let k v)            = (TLet k v)
eval (Define k vs)        = (TDefine k vs)
eval (Comment _)          = (TNoop)

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
