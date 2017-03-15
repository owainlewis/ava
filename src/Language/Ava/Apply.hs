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
-- Modules here are used to apply some Instruction value to a
-- stack and return a result.
--
-- These functions make up the core language def. Note that for now they are
-- implemented inefficiently in Haskell but should be moved to LLVM generated
-- code eventually
--
module Language.Ava.Apply
    ( applyOp )
    where

import           Control.Monad.Except
import qualified Data.Map                              as M
import           Language.Ava.Base.AST
import qualified Language.Ava.Base.AST                 as AST
import           Language.Ava.Instruction
import qualified Language.Ava.Reader      as Rdr
import           Language.Ava.Internal.Stack           (Stack (..))
import qualified Language.Ava.Internal.Stack           as Stack

import Language.Ava.Base.Error(ProgramError(..), failGeneric, failState)

type Result = ExceptT ProgramError IO (Stack AST.Value)

type AvaFunction = Stack Value -> Result

liftPrim :: Prim -> Value
liftPrim x = Prim x

-- | Abstraction for commonly used pattens
--
liftModify :: Monad m => ([a] -> Either e [a]) -> Stack a -> ExceptT e m (Stack a)
liftModify f s = ExceptT . return $ Stack.modifyM f s

-- | Another abstraction for combining return with ExceptT
--
liftExcept :: Monad m => Either e a -> ExceptT e m a
liftExcept = ExceptT . return

-- | Continue execution
proceed :: Monad m => a -> ExceptT e m a
proceed a = ExceptT . return . Right $ a

-- | Terminate execution
terminate :: Monad m => e -> ExceptT e m a
terminate e = ExceptT . return . Left $ e

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
applyOp (TStack) s      = _stack s
applyOp (TUnstack) s    = unstack s
applyOp (TInfra) s      = infra s
applyOp (TMult) s       = numericBinOp s (*) "*"
applyOp (TAdd) s        = numericBinOp s (+) "+"
applyOp (TSub) s        = numericBinOp s (-) "-"
applyOp (TDiv) s        = numericBinOp s (div) "/"
applyOp (TGt) s         = boolBinOp s (>) ">"
applyOp (TLt) s         = boolBinOp s (<) "<"
applyOp (TEq) s         = boolBinOp s (==) "=="
applyOp (TDot) s        = dot s
applyOp (TPrint) s      = printS s
applyOp (TNoop) s       = proceed s

liftOp :: Monad m => a -> ExceptT e m a
liftOp = ExceptT . (return . Right)

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
                       , ("infra"  , TInfra)
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
cons s = liftModify f s
    where f ((Prim (Quotation xs)) : x : ys) =
            return $ ((liftPrim (Quotation (x:xs))):ys)
          f (Prim (List xs) : x : ys) =
            return $ ((liftPrim (List (x:xs))):ys)
          f _ = Left . InvalidState $ "cons"

-- | This is the inverse of cons
--
--   uncons [x] => [x []]
--
uncons :: AvaFunction
uncons s = liftModify f s
    where f (Prim (AST.Quotation (x:xs)) : ys) =
            return $ x : (liftPrim $ Quotation xs) : ys
          f (Prim (AST.List (x:xs)) : ys) =
            return $ x : (liftPrim $ List xs) : ys
          f _ = Left $ TypeError "?" "List or quotation"

-- | Choose between two options based on some boolean value
--
--   true  [P] [Q] branch => P.
--
--   false [P] [Q] branch => Q.
--
choice :: AvaFunction
choice s = liftModify f s
    where f (q : p : (Prim (Boolean b)) : xs)  =
            return $ (if b then p else q) : xs
          f _ = Left . InvalidState $ "choice"

-- | Infra combinator
--
-- L [M] [P] infra => L [N]
--
-- The infra combinator temporarily discards the current stack and takes M as the
-- current stack state. It then executes the top quotation P which returns a new
-- stack N. The new stack N is pushed back as a quotation onto the original stack
--
infra :: AvaFunction
infra stack@(Stack s procs vars) =
    case s of
      (Prim (Quotation p)) : (Prim (Quotation m)) : xs -> do
          Stack ns np nv <- ExceptT $ execute (Stack m procs vars) (map Rdr.eval p)
          proceed $ Stack (Prim (Quotation ns) : xs) np nv
      _ -> terminate $ InvalidState "infra"

-- The stack can be pushed as a quotation onto the stack
--
_stack :: AvaFunction
_stack s = liftExcept (Stack.modifyM (\xs -> return $ liftPrim (Quotation xs) : xs) s)

-- | Unstack
--
unstack :: AvaFunction
unstack s = liftExcept (Stack.modifyM f s)
    where f ((Prim (Quotation q)) : xs) = return q
          f _ = Left . InvalidState $ "unstack"

----------------------------------------------------------------

numericBinOp :: Stack Value -> (Integer -> Integer -> Integer) -> String -> Result
numericBinOp s op opName = liftModify f s
    where f (Prim (Integer x) : (Prim (Integer y)) : xs) =
              return $ liftPrim (Integer (x `op` y)) : xs
          f _ = Left . InvalidState $ unwords ["binary operation", opName]

boolBinOp :: Stack Value -> (Value -> Value -> Bool) -> String -> Result
boolBinOp s op opName = liftModify f s
    where f (x : y : xs) =
            return $ liftPrim (Boolean (x `op` y)) : xs
          f _ = Left . InvalidState $ unwords ["binar operation", opName]

-----------------------------------------------------------------
-- IO operations

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
