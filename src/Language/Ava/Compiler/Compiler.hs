{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.Compiler where

import qualified Language.Ava.Base.Reader              as BaseReader
import qualified Language.Ava.Intermediate.Reader      as Reader

data CompilerStage = LexStage
                   | ParseStage
                   | CodeGen






