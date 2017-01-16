{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.Base.Main where

import qualified Language.Ava.Parser as Parser
import qualified Data.Text as T

main = Parser.parseMany . T.pack
