module Language.Seven.Std.Base
    ( symTab)
    where

import Control.Monad.State
import qualified Data.Map               as M
import           Language.Seven.AST
import qualified Language.Seven.Machine as Machine

-- | Standard Library
-- ----------------------------------------------------------

-- | Boolean operators
--
-- AND Push the result of the logical AND of the top two items.
-- OR Push the result of the logical OR of the top two items.
-- NOT Push the result of the logical NOT of the top item.
-- NAND Push the result of the logical NAND of the top two items.
-- NOR Push the result of the logical NOR of the top two items

symTab :: M.Map String (Machine.VM ())
symTab = M.fromList [ ("+", binOp (+))
                    , ("-", binOp (-))
                    , ("*", binOp (*))
                    -- Stack operations
                    , ("UNIT", unit)
                    , ("APPLY", apply)
                    , ("DEBUG", debug)
                    , ("SWAP", swap)
                    , ("POP", pop)
                    , ("DUP", dup)
                    ]

-- | Apply a binary operation to two elements on the stack
--
binOp :: (Int -> Int -> Int) -> Machine.VM ()
binOp op = do
    Machine.withArity 2
    x <- Machine.pop
    y <- Machine.pop
    case (x,y) of
        (Integer x1, Integer y1) -> Machine.push $ Integer (x1 `op` y1)
        _ -> Machine.raise $ Machine.RuntimeException "Expecting two integers"

debug :: Machine.VM ()
debug = do
    rt <- Machine.getRuntime
    liftIO . print . show $ rt

-- | Stack operations
--
-- SWAP Swap the first two items on the stack
-- DUP Duplicate the top item on the stack
-- POP Remove the first item on the stack
-- CAT
-- CONS
-- UNIT
-- APPLY unquotes the elements in a quotation and applies them to the stack

-- [B] [A] swap == [A] [B]
--     [A] dup  == [A] [A]
--     [A] zap  ==
-- [B] [A] cat  == [B A]
-- [B] [A] cons == [[B] A]
--     [A] unit == [[A]]
--     [A] i   == A
-- [B] [A] dip == A [B]

swap :: Machine.VM ()
swap = do
    x <- Machine.pop
    y <- Machine.pop
    Machine.push x
    Machine.push y

dup :: Machine.VM ()
dup = do
    xs <- Machine.getRuntime
    case xs of
        (x:xs) -> Machine.setRuntime (x : x : xs)
        _ -> Machine.noop

pop :: Machine.VM ()
pop = do
    xs <- Machine.getRuntime
    case xs of
        (x:xs) -> Machine.setRuntime xs
        _ -> Machine.noop

unit :: Machine.VM ()
unit = do
    xs <- Machine.getRuntime
    case xs of
        (x:xs) -> Machine.setRuntime (Vector [x] : xs)
        _ -> Machine.noop

apply :: Machine.VM ()
apply = do
    xs <- Machine.getRuntime
    case xs of
        (Vector ys:xs) -> Machine.setRuntime (ys ++ xs)
        _ -> Machine.noop
