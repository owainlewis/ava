{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.Base.Main where

import qualified Data.Text                   as T
import qualified Language.Ava.Base.Parser    as Parser

import qualified Language.Ava.Base.Stack     as Stack

import           Language.Ava.Base.AST
import           Language.Ava.Base.Execution

eval (Integer x)     = TPush (Integer x)
eval (Float x)       = TPush (Float x)
eval (String x)      = TPush (String x)
eval (List x)        = TPush (List x)
eval (Quotation x)   = TPush (Quotation x)
eval (Boolean b)     = TPush (Boolean b)
eval (Word "dup")    = TDup
eval (Word "cons")   = TCons
eval (Word "uncons") = TUncons
eval (Word "pop")    = TPop
eval (Word "choice") = TChoice
eval (Word "let")    = TLet

transformProgram :: T.Text -> [PrimOp]
transformProgram input =
    case (Parser.parseMany input) of
        Right p-> map eval p
        Left e  -> []

run p = let instructions = transformProgram (T.pack p) in
        executeF Stack.empty instructions


