module Language.Ava.Std.Base (symTab) where

import           Control.Monad.State
import qualified Data.Map             as M
import           Language.Ava.AST
import qualified Language.Ava.Machine as Machine

-- --------------------------------------------------------------
-- | Standard Library
-- ----------------------------------------------------------

type Operation = (String, Machine.VM ())

symTab :: M.Map String (Machine.VM ())
symTab = M.fromList allOperations
    where allOperations =
            baseOperations ++
            stackOperations ++
            miscOperations

baseOperations :: [Operation]
baseOperations = [ ("PLUS", binOp (+))
                 , ("MINUS", binOp (-))
                 , ("TIMES", binOp (*))
                 , ("EQ", binOpBoolean (==))
                 , ("GT", binOpBoolean (>))
                 , ("LT", binOpBoolean (<))
                 , ("LTE", binOpBoolean (<=))
                 , ("GTE", binOpBoolean (>=))
                 , ("AND", logicalOperator (\x y -> x && y))
                 , ("OR", logicalOperator (\x y -> x || y))
                 ]

miscOperations :: [Operation]
miscOperations = [ ("DEBUG", debug)
                 , ("PRINT", printTop)
                 ]

stackOperations :: [Operation]
stackOperations = [ ("UNIT", unit)
                  , ("APPLY", apply)
                  , ("SWAP", swap)
                  , ("DUP", dup)
                  , ("POP", pop)
                  ]

debug :: Machine.VM ()
debug = do
    rt <- Machine.getRuntime
    liftIO . print $ rt

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

printTop :: Machine.VM ()
printTop = do
    rt <- Machine.getRuntime
    case safeHead rt of
        Just v  -> liftIO . print $ v
        Nothing -> return ()

-- Swap the first two items on the stack
--
-- [B] [A] SWAP == [A] [B]
--
swap :: Machine.VM ()
swap = do
    x <- Machine.pop
    y <- Machine.pop
    Machine.push x
    Machine.push y

-- Duplicate the top item on the stack
--
-- [A] DUP == [A] [A]
--
dup :: Machine.VM ()
dup = Machine.modifyRuntime f
    where f (x:xs) = (x : x : xs)
          f []     = []

-- Remove the first item on the stack
--
-- [A] POP ==
--
pop :: Machine.VM ()
pop = Machine.modifyRuntime f
    where f []     = []
          f (x:xs) = xs

-- Given an item on the stack, turn it into a list
--
-- [A] UNIT == [[A]]
--
unit :: Machine.VM ()
unit = Machine.modifyRuntime f
    where f (x:xs) = Vector [x] : xs
          f []     = []

-- Given a list of elements, apply them to the stack
--
-- [A] APPLY == A
--
apply :: Machine.VM ()
apply = do
    xs <- Machine.getRuntime
    case xs of
        (Vector ys:xs) -> Machine.setRuntime (ys ++ xs)
        _              -> Machine.noop

-- | Con(cat)enate two lists together
--
 -- [B] [A] CAT == [B A]
--
cat :: Machine.VM ()
cat = do
    runtime <- Machine.getRuntime
    case runtime of
        (Vector xs) : (Vector ys) : rest -> Machine.setRuntime $ (Vector (xs ++ ys)) : rest
        _ -> Machine.noop

-- Cons an element onto a list
--
-- [B] [A] cons == [[B] A]
--
cons :: Machine.VM ()
cons = do
    runtime <- Machine.getRuntime
    case runtime of
      (x : Vector xs : ys) -> Machine.setRuntime $ (Vector (x : xs)) : ys
      _                    -> Machine.noop

-- TODO
--
-- [B] [A] dip == A [B]
--
dip :: Machine.VM ()
dip = Machine.noop

-- --------------------------------------------------------------
-- | Basic comparison and equality operations.
-- --------------------------------------------------------------

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
        _ -> Machine.raise $ Machine.TypeError "Expecting two integers"

-- | Utility for constructing binary operations that return a boolean value
--
binOpBoolean :: (Value -> Value -> Bool) -> Machine.VM ()
binOpBoolean op = do
    Machine.withArity 2
    x <- Machine.pop
    y <- Machine.pop
    Machine.push $ Boolean (x `op` y)

-- | Utility for constructing binary operations that require and produce boolean values
--
logicalOperator :: (Bool -> Bool -> Bool) -> Machine.VM ()
logicalOperator op = do
    Machine.withArity 2
    x <- Machine.pop
    y <- Machine.pop
    case (x,y) of
        (Boolean b1, Boolean b2) -> Machine.push $ Boolean (b1 `op` b2)
        _ -> Machine.raise $ Machine.TypeError "Expecting two boolean values"
