-- |
-- Module      : Language.Ava.Eval
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Evaluates a input from the parser and returns an operation / instruction
--
module Language.Ava.Eval
    ( eval )
    where

import           Language.Ava.AST
import           Language.Ava.Execution

import           Data.Semigroup((<>))
import           Data.Maybe(maybe)

import qualified Language.Ava.Stack as S
import qualified Data.Map as M

liftO :: Applicative f => a -> f [a]
liftO x = pure [ x ]

-- Simplify a complex user defined procedure into a sequence of simple
-- primary operations
--
simplifyProcedure :: [Value] -> Either ProgramError [Instruction]
simplifyProcedure p = foldl (\acc v -> let e = eval v in
                                       case e of
                                         Left e -> Left e
                                         Right xs ->
                                           case acc of
                                             Left e -> Left e
                                             Right ys -> Right $ xs ++ ys)
                                    (Right []) p

-- | A transform operation from some value to a sequence of prim operations
--
--   A value that returns `Nothing` is a value that cannot be interpreted
--
eval :: Value -> Either ProgramError [Instruction]

eval (Integer x)      = liftO (TPush (Integer x))
eval (Float x)        = liftO (TPush (Float x))
eval (String x)       = liftO (TPush (String x))
eval (List x)         = liftO (TPush (List x))
eval (Quotation x)    = liftO (TPush (Quotation x))
eval (Boolean b)      = liftO (TPush (Boolean b))
eval (Procedure x xs) = simplifyProcedure xs
-- Here be dragons
eval (Word w)         = maybe (liftO (TPush (Word w)))
                             (\op -> liftO op)
                             (M.lookup w allWords)
    where allWords = M.fromList [ ("dup", TDup)
                                , ("cons", TCons)
                                , ("uncons", TUncons)
                                , ("pop", TPop)
                                , ("choice", TChoice)
                                , ("let", TLet)
                                ]
