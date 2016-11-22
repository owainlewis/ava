{-# LANGUAGE FlexibleContexts #-}
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

pushVM :: VM -> Value -> VM
pushVM vm@(VM _ _ output) x = vm { output = x : output }

push :: Value -> StateT VM IO (Maybe Value)
push v = do
  vm <- get
  modify (flip pushVM v)
  return Nothing

pop :: StateT VM IO (Maybe Value)
pop = do
  vm <- get
  put vm
  return (Just (Word "OK"))

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
