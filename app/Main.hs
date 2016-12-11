module Main
  ( main
  , repl
  ) where

import qualified Data.Map              as M
import           Language.Seven.AST    (Value (..))
import           Language.Seven.Core   (Stack (..), eval, evalS)
import           Language.Seven.Parser (parseSeven)
import           System.IO             (hFlush, stdout)

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
    runRepl (Stack [] (M.empty))

-- | Run the REPL loop providing the current state of the world
runRepl :: Stack ->  IO ()
runRepl stackState = do
    putStr "REPL> "
    hFlush stdout
    ln <- getLine
    hFlush stdout
    if ln == "exit()"
      then return ()
      else
        case (parseSeven ln) of
            Left e -> do
                putStrLn . show $ e
                runRepl stackState
            Right result -> do
                (outcome, stack) <- evalS result stackState
                runRepl stack
                return ()

-- | The main entry point is used to run a seven program from file
--
runProgram :: IO ()
runProgram = do
  contents <- readFile "program.seven"
  case (parseSeven contents) of
      Right program ->
          eval program >> return ()
      Left err ->
          putStrLn ("Failed to parse program" ++ show err)
  return ()

main :: IO ()
main = repl
