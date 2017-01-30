-- |
-- Module      : Language.Ava.Apply
--
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : Owain Lewis <owain@owainlewis.com>
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

-- | Takes an instruction and turns it into a function that can be applied to a stack
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
applyOp (AST.TApply w) s = applyWord w s
applyOp (AST.TLet k v) s = error "TODO"
applyOp (AST.TDefine k v) s = define k v s

define :: Monad m => String -> [a] -> Stack a -> ExceptT e m (Stack a)
define k v stack@(Stack s procs vars) =
  liftOp $ Stack.setProcedure stack k v

-- | Apply a word by resolving it's meaning from the current stack.
--
--   Words are derived in the following order:
--
--   1. Look for a procedure named w
--   2. Look for  a var named w
--   3. Look for a native word named w
--   4. Word is unbound
--
applyWord :: String -> Stack AST.Value ->
             ExceptT AST.ProgramError IO (Stack AST.Value)
applyWord w stack@(Stack s p v) =
    -- Start with procedures.
    -- Apply a procedure is basically taking the values
    -- and applyin then evaluating them
    case (M.lookup w p) of
      Just steps -> do
        -- This might work? Just adding the values back onto the stack
        liftOp $ Stack (steps ++ s) p v
        failOp (AST.GenericError "TODO")
      Nothing -> case (M.lookup w allWords) of
                   Just g -> applyOp g stack
                   Nothing -> failOp (AST.GenericError $ "Unbound word " ++ w)
    where allWords =
            M.fromList [ ("dup", AST.TDup)
                       , ("cons", AST.TCons)
                       , ("uncons", AST.TUncons)
                       , ("pop", AST.TPop)
                       , ("choice", AST.TChoice)
                       ]

-- | -----------------------------------------------------------

-- | TODO I think these are already defined in mtl ? Being stupid
liftOp :: Monad m => a -> ExceptT e m a
liftOp = ExceptT . (return . Right)

failOp :: Monad m => e -> ExceptT e m a
failOp e = ExceptT . (return . Left) $ e

-- | -----------------------------------------------------------

-- | Push a value onto the stack
--
--   push x [y] => [x y]
--
push :: Monad m => t -> Stack t -> ExceptT e m (Stack t)
push v s = liftOp $ Stack.modify (\s -> v:s) s

-- | Pop an item off the stack
--
--   pop [x y] => [y]
--
pop :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
pop s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = xs

-- | Duplicate the top item on the stack
--
--   dup [x] => [x x]
--
dup :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
dup s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = (x:x:xs)

-- | Swap the first two elements on the stack
--
--   swap [x y] => [y x]
--
swap :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
swap s = liftOp $ Stack.modify f s
    where f (x:y:xs) = y:x:xs
          f x        = x

-- | Cons some value onto a list of procedure
--
--   cons x [] => [x y]
--
cons :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
cons s = ExceptT . return $ Stack.modifyM f s
    where f (x : AST.Quotation xs : ys) =
              return $ (AST.Quotation (x : xs)) : ys
          f (x : AST.List xs : ys) =
            return $ (AST.List (x : xs)) : ys
          f _ = Left . AST.InvalidState $ "cons"

-- | This is the inverse of cons
--
--   uncons [x] => [x []]
--
uncons :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
uncons s = ExceptT . return $ Stack.modifyM f s
    where f (AST.Quotation (x:xs) : ys) =
            return $ x : (AST.Quotation xs) : ys
          f (AST.List (x:xs) : ys) =
            return $ x : (AST.List xs) : ys
          f _ = Left . AST.InvalidState $ "uncons"

-- | Choose between two options based on some boolean value
--
choice :: Stack AST.Value -> ExceptT AST.ProgramError IO (Stack AST.Value)
choice s = ExceptT . return $ Stack.modifyM f s
    where f ((AST.Boolean b) : y : n : xs)  =
            if b then return $ y : xs
                 else return $ n : xs
          f _ = Left . AST.InvalidState $ "choice"

stack = "TODO"

unstack = "TODO"

infra = "TODO"

