module Main
  ( main
  , repl
  ) where

import qualified Data.Map               as M
import           Language.Seven.AST
import qualified Language.Seven.Eval    as Eval
import qualified Language.Seven.Machine as Machine
import qualified Language.Seven.Parser  as Parser
import           System.IO              (hFlush, stdout)

-- | Run a (R)ead (E)val (P)rint (L)oop top level starting with an empty state
--
repl :: IO ()
repl =
  let intro = ["Seven Language"
              , ""
              , "Press exit() to quit"
              , ""
              ] in
  do
    mapM_ putStrLn intro
    runRepl (Machine.Stack [] (M.empty))

-- | Run the REPL loop providing the current state of the world
runRepl :: Machine.Stack ->  IO ()
runRepl stackState = do
    putStr "REPL> "
    hFlush stdout
    ln <- getLine
    hFlush stdout
    if ln == "exit()"
      then return ()
      else
        case (Parser.parseSeven ln) of
            Left e -> do
                putStrLn . show $ e
                runRepl stackState
            Right result -> do
                (outcome, stack) <- Eval.evalS result stackState
                runRepl stack
                return ()

-- | The main entry point is used to run a seven program from file
--
runProgram :: IO ()
runProgram = do
  contents <- readFile "program.seven"
  case (Parser.parseSeven contents) of
      Right program ->
          Eval.eval program >> return ()
      Left err ->
          putStrLn ("Failed to parse program" ++ show err)
  return ()

main :: IO ()
main = repl
