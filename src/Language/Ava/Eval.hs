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
import qualified Language.Ava.Stack as S
import           Data.Maybe(maybe)
import qualified Data.Map as M

liftO :: Applicative f => a -> f [a]
liftO x = pure [ x ]

-- Simplify a complex user defined procedure into a sequence of simple
-- primary operations
--
simplifyProcedure :: [Value] -> Either ProgramError [PrimOp]
simplifyProcedure p = foldl (\acc v -> let e = eval v in
                                       case e of
                                         Left e -> Left e
                                         Right xs ->
                                           case acc of
                                             Left e -> Left e
                                             Right ys -> Right $ xs ++ ys)
                                    (Right []) p


-- | A transform operation from some value to a sequence of operations
--
--   A value that returns `Nothing` is a value that cannot be interpreted
--
eval :: Value ->
        Either ProgramError [PrimOp]
eval (Integer x)      = liftO (TPush (Integer x))
eval (Float x)        = liftO (TPush (Float x))
eval (String x)       = liftO (TPush (String x))
eval (List x)         = liftO (TPush (List x))
eval (Quotation x)    = liftO (TPush (Quotation x))
eval (Boolean b)      = liftO (TPush (Boolean b))
eval (Procedure x xs) = simplifyProcedure xs
eval (Word w)         = maybe (Left $ GenericError "Unbound word")
                             (\op -> liftO op)
                             (M.lookup w allWords)
    where allWords = M.fromList [ ("dup", TDup)
                                , ("cons", TCons)
                                , ("uncons", TUncons)
                                , ("pop", TPop)
                                , ("choice", TChoice)
                                , ("let", TLet)
                                ]
