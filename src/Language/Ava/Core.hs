-- |
-- Module      : Language.Ava.Core
-- Copyright   : (c) 2016 Owain Lewis
--
-- License     : BSD-style
-- Maintainer  : owain@owainlewis.com
-- Stability   : experimental
-- Portability : GHC
--
-- Core language operations that form the base language upon which
-- everything else is built
--
module Language.Ava.Core
    (language)
    where

import           Control.Monad.State
import qualified Data.Map             as M
import           Language.Ava.AST
import qualified Language.Ava.Machine as Machine

type VMState = Machine.VM ()

language :: M.Map String VMState
language = M.union core internal

-- | Towards a smaller core language (mostly derived from Joy)
core :: M.Map String VMState
core = M.fromList
    [ ("choice", choice)
    , ("infra", infra)
    , ("stack", stack)
    , ("unstack", unstack)
    , ("cons", cons)
    , ("uncons", uncons)
    , ("swap", swap)
    , ("dup", dup)
    , ("pop", pop)
    , (">", gt)
    , ("<", lt)
    , ("=", eq)
    , ("+", plus)
    , ("-", minus)
    , ("*", times)
    ]

-- Selects one of two possible outcomes
--
-- X Y B CHOICE == (X | Y)
choice :: Machine.VM ()
choice = do
  runtime <- Machine.getRuntime
  case runtime of
    (y : x : (Boolean cond) : xs) ->
      Machine.setRuntime $ if cond then x : xs else y : xs
    _ -> Machine.raise $ Machine.TypeError "Invalid state for operation `choice`"


infra :: Machine.VM ()
infra = Machine.noop

-- The stack can be pushed as a quotation onto the stack by stack
stack :: Machine.VM ()
stack = Machine.noop

-- A quotation can be turned into the stack with unstack
--
unstack :: Machine.VM()
unstack = do
    runtime <- Machine.getRuntime
    case runtime of
      (Quotation xs) : ys -> Machine.setRuntime xs
      _ -> Machine.raise $ Machine.TypeError "Invalid state for operation: `unstack`"

-- Swap the first two items on the stack
--
-- B A SWAP == A B
--
swap :: Machine.VM ()
swap = do
    x <- Machine.pop
    y <- Machine.pop
    Machine.push x
    Machine.push y

-- Duplicate the top item on the stack
--
-- A DUP == A A
--
dup :: Machine.VM ()
dup = Machine.modifyRuntime f
    where f (x:xs) = (x : x : xs)
          f []     = []

-- Cons an element onto a list or quotation
--
-- B [A] cons == [B A]
--
cons :: Machine.VM ()
cons = do
    runtime <- Machine.getRuntime
    case runtime of
      (x : Quotation xs : ys) -> Machine.setRuntime $ (Quotation (x : xs)) : ys
      (x : List xs : ys) -> Machine.setRuntime $ (List (x : xs)) : ys
      _                    -> Machine.raise $ Machine.TypeError msg
          where msg = "Invalid state for operation: `cons`"

uncons :: Machine.VM ()
uncons = Machine.noop

-- Remove the first item on the stack
--
-- A POP ==
--
pop :: Machine.VM ()
pop = Machine.modifyRuntime f
    where f []     = []
          f (x:xs) = xs

-- | Utility for constructing binary operations that return a boolean value
--
binOpBoolean :: (Value -> Value -> Bool) -> Machine.VM ()
binOpBoolean op = do
    Machine.withArity 2
    x <- Machine.pop
    y <- Machine.pop
    Machine.push $ Boolean (x `op` y)

------------------------------------------
-- | Types
------------------------------------------

-- Boolean (true | false)
-- Integer (1, 2 ..)
-- Float (1.25)
-- String ("foobar")
-- Char ('a)
-- List [...]
-- Set #{...}
-- Map {:k v}

------------------------------------------
-- Math
------------------------------------------

-- + : X Y -> Z
-- Add two integers
plus = binOp (+)

-- - : X Y -> Z
-- Subtract Y from X
minus = binOp (-)

-- * : X Y -> Z
-- Multiply two integers
times = binOp(*)

-- | Apply a binary operation to two elements on the stack
--
binOp :: (Int -> Int -> Int) -> Machine.VM ()
binOp op = do
    Machine.withArity 2
    x <- Machine.pop
    y <- Machine.pop
    case (x,y) of
        (Integer x1, Integer y1) ->
            Machine.push $ Integer (x1 `op` y1)
        _ -> Machine.raise $ Machine.TypeError msg
        where msg = "Invalid state for binary operation. Expecting two integers"

------------------------------------------
-- Predicates
------------------------------------------

gt, lt, eq :: Machine.VM ()

-- > : X Y -> B
-- Tests whether X greater than Y
gt = binOpBoolean (>)

-- < : X Y -> B
-- Tests whether X less than Y
lt = binOpBoolean (<)

-- != : X Y -> B
-- Tests whether X is equal to Y
eq = binOpBoolean (==)

-- integer? : X -> B
-- Tests whether X is an integer.

-- float? : R -> B
-- Tests whether R is a float.

-- char? : X  ->  B
-- Tests whether X is a character.

-- string? : X -> B
-- Tests whether X is a string.

-- list? : X -> B
-- Tests whether X is a list

-- set? : X -> B
-- Tests whether X is a set

-- map? : X -> B
-- Tests whether X is a map

internal :: M.Map String VMState
internal = M.fromList
    [ (":debug", debug)
    , (":clear", clear)
    , (":procs", procs)
    , (":vars", vars)
    ]

procs :: Machine.VM ()
procs = (liftIO . print) =<< Machine.getProcedureNames

vars :: Machine.VM ()
vars = (liftIO . print) =<< Machine.getVarNames

-- | Dumps the current stack state and inspect the current state
--
debug :: Machine.VM ()
debug = (liftIO . print) =<< Machine.getRuntime

-- | Flush the current runtime
--
clear :: Machine.VM ()
clear = Machine.setRuntime []
