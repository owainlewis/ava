module Language.Seven.Core
    ( runInstr
    , run
    ) where

import Control.Lens

data Value = Word String
           | Integer Int
  deriving ( Show )

data VM = VM {
    pointer :: Int
  , input :: [Value]
  , output :: [Value]
} deriving ( Show )

data Instruction = Push Value | Pop | GoTo Int

mkVM :: [Value] -> VM
mkVM input = VM { pointer = 0
                , input = input
                , output = []
                }

runInstr :: Instruction -> t -> t
runInstr (Push v) vm = vm

run :: Value -> t -> Instruction
run (Integer n) vm = Push (Integer n)
run _ _ = error "Not implemented"
