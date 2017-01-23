{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Language.Ava.Main
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Brings together all the parts
--
module Language.Ava.Main where

import qualified Data.Text              as T
import qualified Language.Ava.Parser    as Parser
import qualified Language.Ava.Stack     as Stack
import qualified Language.Ava.Eval      as Eval
import qualified Language.Ava.AST       as AST

import Data.Maybe(maybe)

import           Language.Ava.Execution

-- The idea here is to take a complex program and reduce it down
-- into a series of simple operations.
--
--
transformProgram :: T.Text -> Either AST.ProgramError [AST.PrimOp]
transformProgram input =
    case (Parser.parseMany input) of
        Right p -> foldl (\acc value ->
                            -- Fail early
                            case acc of
                              Left e -> Left e
                              Right pops ->
                                case (Eval.eval value) of
                                  Left e -> Left e
                                  Right ops -> Right $ acc ++ ops)
                   (Right [])
                   p
        Left e  -> Left $ AST.GenericError "Failed to parse program"

run p = let instructions = transformProgram (T.pack p) in
        case instructions of
          Left e -> putStrLn "Failed"
          Right instr -> execute Stack.empty instr >> return ()


