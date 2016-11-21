module Language.Seven.Core where

data Value = Word String
           | Integer Int
  deriving ( Show )

data VM = VM {
    input :: [Value]
  , output :: [Value]
} deriving ( Show )

data Instruction = Push Value | Pop

mkVM :: [Value] -> VM
mkVM input = VM { input = input, output = [] }

run :: Value -> t -> Instruction
run (Integer n) vm = Push (Integer n)
run _ _ = error "Not implemented"
