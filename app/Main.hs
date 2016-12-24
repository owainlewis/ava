{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  , repl
  ) where

import qualified Data.Map             as M
import           Language.Ava.AST
import qualified Language.Ava.Eval    as Eval
import qualified Language.Ava.Machine as Machine
import qualified Language.Ava.Parser  as Parser
import           System.IO            (hFlush, stdout)

import qualified Data.Text            as T
import qualified Data.Text.IO         as TextIO

-- | Run a (R)ead (E)val (P)rint (L)oop top level starting with an empty state
--
repl :: IO ()
repl =
  let intro = ["Ava Language"
              , ""
              , "Press exit() to quit"
              , ""
              ] in
  do
    mapM_ putStrLn intro
    runRepl (Machine.Stack [] (M.empty) (M.empty))

-- | Run the REPL loop providing the current state of the world
runRepl :: Machine.Stack ->  IO ()
runRepl stackState = do
    putStr "REPL> "
    hFlush stdout
    ln <- TextIO.getLine
    hFlush stdout
    if ln == "exit()"
      then return ()
      else
        case (Parser.parseMany ln) of
            Left e -> do
                putStrLn . show $ e
                runRepl stackState
            Right result -> do
                (outcome, stack) <- Eval.evalS result stackState
                runRepl stack
                return ()

-- | The main entry point is used to run a seven program from file
--
runProgram :: FilePath -> IO ()
runProgram p = do
  contents <- TextIO.readFile p
  case (Parser.parseMany contents) of
      Right program ->
          Eval.eval program >> return ()
      Left err ->
          putStrLn ("Failed to parse program" ++ show err)
  return ()

main :: IO ()
main = runProgram "lib/scratch.ava"
