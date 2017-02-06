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
-- These functions make up the core language def. Note that for now they are
-- implemented inefficiently in Haskell but should be moved to LLVM or ASM generated
-- code eventually
--
module Language.Ava.Apply
    ( applyOp )
    where

import           Control.Monad.Except
import qualified Data.Map                              as M
import           Language.Ava.Base.AST
import qualified Language.Ava.Base.AST                 as AST
import           Language.Ava.Intermediate.Instruction
import qualified Language.Ava.Intermediate.Reader      as Rdr
import           Language.Ava.Internal.Stack           (Stack (..))
import qualified Language.Ava.Internal.Stack           as Stack

import Language.Ava.Base.Error(ProgramError(..), failGeneric, failState)

type Result = ExceptT ProgramError IO (Stack AST.Value)

type AvaFunction = Stack Value -> Result

-- | Execute a sequence of intructions in the context of a given stack
--
execute :: Stack Value -> [Instruction] -> IO (Either ProgramError (Stack Value))
execute s ops = runExceptT (foldM (\s f -> applyOp f $ s) s ops)

-- | Takes a series of instructions and runs the on the empty stack
--
execute1 :: [Instruction] -> IO (Either ProgramError (Stack Value))
execute1 = execute Stack.empty

-- | -----------------------------------------------------------

-- | Takes an instruction and turns it into a function that can be applied to a stack
--
applyOp :: Instruction -> Stack AST.Value -> Result
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

applyOp (TStack) s      = error "Not implemented"
applyOp (TUnstack) s    = unstack s
applyOp (TInfra) s      = error "Not implemented"

applyOp (TMult) s       = numericBinOp s (+)
applyOp (TAdd) s        = numericBinOp s (+)
applyOp (TSub) s        = numericBinOp s (-)
applyOp (TDiv) s        = numericBinOp s (div)

applyOp (TGt) s         = boolBinOp s (>)
applyOp (TLt) s         = boolBinOp s (<)
applyOp (TEq) s         = boolBinOp s (==)

applyOp (TDot) s        = dot s
applyOp (TPrint) s      = printS s

-- | Bind a procedure in the current stack
--
define :: Monad m => String -> [a] -> Stack a -> ExceptT e m (Stack a)
define k v s = liftOp $ Stack.setProcedure k v s

-- | Bind a variable in the current stack
--
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
-- TODO this is a horrible mess. Also check for conflicts in naming !!
--
applyWord :: String -> Stack AST.Value -> Result
applyWord w stack@(Stack s p v) =
    -- Start with procedures.
    -- Apply a procedure is basically taking the values
    -- and applyin then evaluating them
    case (M.lookup w p) of
      Just steps -> do
          ExceptT $ execute stack (map Rdr.eval steps)
      Nothing ->
          case (Stack.getVar w stack) of
              Just var -> applyOp (Rdr.eval var) stack
              Nothing -> case (M.lookup w allWords) of
                   Just native -> applyOp native stack
                   Nothing -> ExceptT . failGeneric $ "Unbound word " ++ w
    where allWords =
            M.fromList [ ("pop"    , TPop)
                       , ("swap"   , TSwap)
                       , ("dup"    , TDup)
                       , ("cons"   , TCons)
                       , ("uncons" , TUncons)
                       , ("choice" , TChoice)
                       , ("stack"  , TStack)
                       , ("unstack", TUnstack)
                       , ("*"      , TMult)
                       , ("+"      , TAdd)
                       , ("-"      , TSub)
                       , ("/"      , TDiv)
                       , (">"      , TGt)
                       , ("<"      , TLt)
                       , ("="      , TEq)
                       , ("."      , TDot)
                       , ("print"  , TPrint)
                       ]

liftOp :: Monad m => a -> ExceptT e m a
liftOp = ExceptT . (return . Right)

-- | -----------------------------------------------------------

-- | Push a value onto the stack
--
--   push x [y] => [x y]
--
push :: Monad m => a -> Stack a -> ExceptT e m (Stack a)
push v = liftOp . Stack.modify (v:)

-- | Pop an item off the stack
--
--   pop [x y] => [y]
--
pop :: AvaFunction
pop s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = xs

-- | Duplicate the top item on the stack
--
--   dup [x] => [x x]
--
dup :: AvaFunction
dup s = liftOp $ Stack.modify f s
        where f []     = []
              f (x:xs) = (x:x:xs)

-- | Swap the first two elements on the stack
--
--   swap [x y] => [y x]
--
swap :: AvaFunction
swap s = liftOp $ Stack.modify f s
    where f (x:y:xs) = y:x:xs
          f x        = x

-- | Cons some value onto a list of procedure
--
--   cons x [] => [x y]
--
cons :: AvaFunction
cons s = ExceptT . return $ Stack.modifyM f s
    where f (x : AST.Quotation xs : ys) =
              return $ (AST.Quotation (x : xs)) : ys
          f (x : AST.List xs : ys) =
            return $ (AST.List (x : xs)) : ys
          f _ = Left . InvalidState $ "cons"

-- | This is the inverse of cons
--
--   uncons [x] => [x []]
--
uncons :: AvaFunction
uncons s = ExceptT . return $ Stack.modifyM f s
    where f (AST.Quotation (x:xs) : ys) =
            return $ x : (AST.Quotation xs) : ys
          f (AST.List (x:xs) : ys) =
            return $ x : (AST.List xs) : ys
          f _ = Left $ TypeError "?" "List or quotation"

-- | Choose between two options based on some boolean value
--
choice :: AvaFunction
choice s = ExceptT . return $ Stack.modifyM f s
    where f (n : y : (AST.Boolean b) : xs)  =
            if b then return $ y : xs
                 else return $ n : xs
          f _ = Left . InvalidState $ "choice"

-- |
stack = "TODO"

unstack :: AvaFunction
unstack s = ExceptT . return $ Stack.modifyM f s
    where f ((AST.Quotation q) : xs) = return q
          f _ = Left . InvalidState $ "unstack"

-- Takes a quotation, executes it and replaces the stack with it
infra = "TODO"

numericBinOp :: Stack Value -> (Int -> Int -> Int) -> Result
numericBinOp s op = ExceptT . return $ Stack.modifyM f s
    where f ((AST.Integer x) : (AST.Integer y) : xs) =
              return $ AST.Integer (x `op` y) : xs
          f _ = Left . InvalidState $ "binary operation"

boolBinOp :: Stack Value -> (Value -> Value -> Bool) -> Result
boolBinOp s op = ExceptT . return $ Stack.modifyM f s
    where f (x : y : xs) =
            return $ AST.Boolean (x `op` y) : xs
          f _ = Left . InvalidState $ "binary operation"

dot :: AvaFunction
dot s@(Stack vs _ _)  = do
    liftIO . putStrLn . show $ vs
    liftOp s

printS :: AvaFunction
printS s@(Stack vs _ _) =
    case vs of
      [] -> liftOp s
      (x:xs) -> do
          liftIO . putStrLn . show $ x
          liftOp s

