{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.Compiler where

import qualified Language.Ava.Base.Reader              as BaseReader
import qualified Language.Ava.Intermediate.Reader      as Reader

data CompilerStage = LexStage
                   | ParseStage
                   | CodeGen
<<<<<<< 3f0c81c35bd52e9434603409f43119eb794e86ef
=======



>>>>>>> Updates compiler to load std lib
