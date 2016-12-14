module Language.Seven.Std.Base
    ( symTab)
    where

import qualified Data.Map               as M
import           Language.Seven.AST
import qualified Language.Seven.Machine as Machine

-- | Standard Library
-- ----------------------------------------------------------

symTab :: M.Map String (Machine.VM ())
symTab = M.fromList [ ("+", binOp (+))
                    , ("-", binOp (-))
                    , ("*", binOp (*))
                    ]

-- | Apply a binary operation to two elements on the stack
--
binOp :: (Int -> Int -> Int) -> Machine.VM ()
binOp op = do
  Machine.withArity 2
  x <- Machine.pop
  y <- Machine.pop
  case (x,y) of
    (Number x1, Number y1) -> Machine.push $ Number (x1 `op` y1)
    _ -> Machine.raise $ Machine.RuntimeException "Expecting two integers"

swap :: Machine.VM ()
swap = do
  x <- Machine.pop
  y <- Machine.pop
  Machine.push x
  Machine.push y

increment :: Machine.VM ()
increment = do
    Machine.withArity 1
    xs <- Machine.getRuntime
    case xs of
      ((Number n):xs) -> Machine.setRuntime ((Number $ n+1):xs)
      _ -> Machine.raise $ Machine.TypeError "Expecting numeric argument"
