module Main ( main) where

import qualified Data.Map              as M
import           Language.Seven.AST    (Value (..))
import           Language.Seven.Core   (Stack (..), eval, evalS)
import           Language.Seven.Parser (parseSeven)

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
    runRepl (Stack [] (M.empty)) True


runRepl :: Stack -> t -> IO ()
runRepl stackState debug = do
    putStr "REPL> "
    ln <- getLine
    if ln == "exit()"
      then return ()
      else
        case (parseSeven ln) of
            Left e -> do
                print e
                runRepl stackState debug
            Right result -> do
                (outcome, stack) <- evalS result stackState
                -- Debug only
                -- print stack
                runRepl stack debug
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
