-- |
-- Module      : Language.Ava.Apply
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Modules here are used to apply some Instruction value to a stack and return a result.
--
-- These functions make up the core language def
--
module Language.Ava.Apply
    ( applyOp )
    where

import           Control.Monad.Except
import           Language.Ava.Stack (Stack (..))

import qualified Data.Map           as M
import qualified Language.Ava.AST   as AST
import qualified Language.Ava.Stack as Stack

-- | -----------------------------------------------------------

-- | Takes an instruction and turns it into a function that can
--   be applied to a stack
--
applyOp :: AST.Instruction ->
           Stack AST.Value ->
           ExceptT AST.ProgramError IO (Stack AST.Value)
applyOp (AST.TPush v) s = push v s
applyOp AST.TPop s = pop s
applyOp AST.TDup s = dup s
applyOp AST.TSwap s = swap s
applyOp AST.TCons s = cons s
applyOp AST.TUncons s = uncons s
applyOp AST.TChoice s = choice s
applyOp AST.TLet s = letOp s

-- | -----------------------------------------------------------

liftOp :: Monad m => a -> ExceptT e m a
liftOp = ExceptT . (return . Right)

-- | -----------------------------------------------------------

-- | Push a value onto the stack
--
push :: Monad m => t -> Stack t -> ExceptT e m (Stack t)
push v s = liftOp $ Stack.modify (\s -> v:s) s

-- | Pop an item off the stack
--
pop :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
pop s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = xs

-- | Duplicate the top item on the stack
--
dup :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
dup s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = (x:x:xs)

-- | Swap the first two elements on the stack
--
swap :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
swap s = liftOp $ Stack.modify f s
    where f (x:y:xs) = y:x:xs
          f x        = x

-- | Cons some value onto a list of procedure
--
cons :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
cons s = ExceptT . return $ Stack.modifyM f s
    where f (x : AST.Quotation xs : ys) =
              return $ (AST.Quotation (x : xs)) : ys
          f (x : AST.List xs : ys) =
            return $ (AST.List (x : xs)) : ys
          f _ = Left . AST.InvalidState $ "cons"

uncons :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
uncons s = ExceptT . return $ Stack.modifyM f s
    where f (AST.Quotation (x:xs) : ys) =
            return $ x : (AST.Quotation xs) : ys
          f (AST.List (x:xs) : ys) =
            return $ x : (AST.List xs) : ys
          f _ = Left . AST.InvalidState $ "uncons"

choice :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
choice s = ExceptT . return $ Stack.modifyM f s
    where f ((AST.Boolean b) : y : n : xs)  =
            if b then return $ y : xs
                 else return $ n : xs
          f _ = Left . AST.InvalidState $ "choice"

stack = "TODO"

unstack = "TODO"

-- | -----------------------------------------------------------

letOp :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
letOp (Stack s p vs) = ExceptT . return $ do
    case s of
        ((AST.Word k) : v : xs) ->
            Right $ Stack xs p (M.insert k v vs)
        _ -> Left $ AST.InvalidState "let"
