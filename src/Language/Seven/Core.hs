{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Seven.Core
    ( runInstr
    , run
    ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Except

data Value = Word String
           | Integer Int
  deriving ( Show )

data VM = VM {
    pointer :: Int
  , input :: [Value]
  , output :: [Value]
} deriving ( Show )

type ProgramError = String

type VMState = Either ProgramError VM

type ProgramState r a = ExceptT ProgramError (State r) a

pushVM :: VM -> Value -> VM
pushVM vm@(VM _ _ output) x = vm { output = x : output }

pop :: State VM (Either String Value)
pop = state $ \vm ->
  case (output vm) of
    x:xs -> (pure x, vm { output = xs})
    _ -> (Left "Stack underflow", vm)

-- pushPop :: StateT VM Identity (Either String Value)
-- pushPop = do
--   push $ Integer 10
--   push $ Integer 20
--   x <- pop
--   return x

-- λ> runState (pushOutput (Word "HELLO")) (mkVM [])
-- ((),VM {pointer = 0, input = [], output = [Word "HELLO"]})

data Instruction = Push Value | Pop | GoTo Int

mkVM :: VM
mkVM = VM { pointer = 0, input = [], output = [] }

runInstr :: Instruction -> t -> t
runInstr (Push v) vm = vm

run :: Value -> t -> Instruction
run (Integer n) vm = Push (Integer n)
run _ _ = error "Not implemented"
