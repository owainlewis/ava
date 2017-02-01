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
import           Language.Ava.Internal.Stack (Stack (..))

import qualified Data.Map           as M
import qualified Language.Ava.Base.AST   as AST
import Language.Ava.Intermediate.Instruction
import qualified Language.Ava.Internal.Stack as Stack

-- | -----------------------------------------------------------

-- | Takes an instruction and turns it into a function that can be applied to a stack
--
applyOp :: Instruction -> Stack AST.Value ->
                          ExceptT AST.ProgramError IO (Stack AST.Value)
applyOp (TPush v) s     = push v s
applyOp (TPop) s        = pop s
applyOp (TDup) s        = dup s
applyOp (TSwap) s       = swap s
applyOp (TCons) s       = cons s
applyOp (TUncons) s     = uncons s
applyOp (TChoice) s     = choice s
applyOp (TApply w) s    = applyWord w s
applyOp (TLet k v) s    = letOp k v s
applyOp (TDefine k v) s = define k v s

define :: Monad m => String -> [a] -> Stack a -> ExceptT e m (Stack a)
define k v s = liftOp $ Stack.setProcedure k v s

letOp :: Monad m => AST.Op -> a -> Stack a -> ExceptT e m (Stack a)
letOp k v s = liftOp $ Stack.setVar k v s

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
      Nothing ->
          case (Stack.getVar w stack) of
              Just var -> liftOp $ Stack s p v
              Nothing -> case (M.lookup w allWords) of
                   Just g -> applyOp g stack
                   Nothing -> failOp (AST.GenericError $ "Unbound word " ++ w)
    where allWords =
            M.fromList [ ("dup", TDup)
                       , ("cons", TCons)
                       , ("uncons", TUncons)
                       , ("pop", TPop)
                       , ("choice", TChoice)
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
