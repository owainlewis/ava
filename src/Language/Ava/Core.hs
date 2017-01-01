module Language.Ava.Core
    (language)
    where

import           Control.Monad.State
import qualified Data.Map             as M
import           Language.Ava.AST
import qualified Language.Ava.Machine as Machine

type VMState = Machine.VM ()

-- | Towards a smaller core language (mostly derived from Joy)
language :: M.Map String VMState
language = M.fromList
    [ ("unstack", Machine.noop)
    , ("choice", choice)
    , ("uncons", Machine.noop)
    , ("infra", Machine.noop)
    , ("stack", Machine.noop)
    , ("swap", swap)
    , ("cons", Machine.noop)
    , ("dup", dup)
    , ("pop", pop)
    , (">", binOpBoolean(>))
    , ("<", binOpBoolean(>))
    , ("==", binOpBoolean(==))
    , (":debug", debug)
    ]

debug :: Machine.VM ()
debug = do
    rt <- Machine.getRuntime
    liftIO . print $ rt

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
      _ -> Machine.raise $ Machine.TypeError "Cannot unstack this type"

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

-- Remove the first item on the stack
--
-- A POP ==
--
pop :: Machine.VM ()
pop = Machine.modifyRuntime f
    where f []     = []
          f (x:xs) = xs

-- Selects one of two possible outcomes
--
-- X Y B CHOICE == (X | Y)
choice :: Machine.VM ()
choice = do
  runtime <- Machine.getRuntime
  case runtime of
    (y : x : (Boolean cond) : xs) ->
      Machine.setRuntime $ if cond then x : xs else y : xs
    _ -> Machine.raise $ Machine.TypeError "Invalid types for operation: choice"

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
-- Core
------------------------------------------

------------------------------------------
-- Predicates
------------------------------------------

-- empty?

-- >= : X Y -> B
-- Tests whether X greater than or equal to Y

-- > : X Y -> B
-- Tests whether X greater than Y

-- <= : X Y -> B
-- Tests whether X less than or equal to Y

-- < : X Y -> B
-- Tests whether X less than Y

-- != : X Y -> B
-- Tests whether X is not equal to Y

-- = : X Y -> B
-- Tests whether X is equal to Y

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
