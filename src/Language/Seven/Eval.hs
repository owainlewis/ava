module Language.Seven.Eval
    ( evalS
    , eval
    ) where

import Control.Exception hiding (TypeError)
import Control.Monad.State
import Language.Seven.AST
import Language.Seven.Machine
import Control.Monad(forM_)

import qualified Language.Seven.Std.Base as Std

import qualified Data.Map as M

-- | Executes a program p (a list of operations to perform in sequential order)
--
-- eval1 [Procedure ">" [Number 20, Number 20, Word "+"], Word ">"]
evalS :: [Value] -> Stack -> IO (Either ProgramError (), Stack)
evalS p stack = run (forM_ p evaluate) stack
    where evaluate (Number n) = push $ Number n
          evaluate (Vector xs) = push $ Vector xs
          evaluate (Boolean b) = push $ Boolean b
          evaluate (String s) = push $ String s
          evaluate (Comment _) = noop
          evaluate (Word w) = do
                -- First we need to check in the current vm env to see if
                -- a user has defined the value of a word w to be some procedure p
                v <- getEnv w
                case v of
                    -- If the value exists then evaluate the procedure
                    Just procedure -> mapM_ evaluate procedure
                    -- Else lookup in the symbol table
                    Nothing ->
                        case (M.lookup w Std.symTab) of
                            Just procedure ->
                                procedure
                            Nothing ->
                                raise $ RuntimeException ("Unbound word " ++ w)
          -- | Evaluate a procedure by updating the environment
          evaluate (Procedure p instrs) = setEnv p instrs

-- | Works like eval but doesn't require an initial input state
--
eval :: [Value] -> IO (Either ProgramError (), Stack)
eval = flip evalS $ Stack [] (M.empty)
