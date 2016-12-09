module Main ( main) where

import Language.Seven.AST(Value(..))
import Language.Seven.Core (Stack(..), eval, evalS)
import Language.Seven.Parser(parseSeven)
import Text.Parsec.Error(ParseError)
import Control.Exception (catch, throwIO, AsyncException(UserInterrupt))
import qualified Data.Map as M
import Control.Monad.State

empty = Stack [] (M.empty)


-- | Run a (R)ead (E)val (P)rint (L)oop top level starting with an empty state
--
repl :: Stack -> IO ()
repl stackState = do
    putStr "REPL>  "
    ln <- getLine
    if ln == "exit"
      then return ()
      else
        case (parseSeven ln) of
            Left e -> do
                print e
                repl stackState
            Right result -> do
                (outcome, stack) <- evalS result stackState
                -- Debug only
                -- print stack
                repl stack
                return ()

-- | The main entry point is used to run a seven program from file
--
main :: IO ()
main = do
  contents <- readFile "program.seven"
  case (parseSeven contents) of
      Right program ->
          eval program >> return ()
      Left err ->
          putStrLn ("Failed to parse program" ++ show err)
  return ()
