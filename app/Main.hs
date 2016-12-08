module Main ( main) where

import Language.Seven.Core as S
import Language.Seven.Parser(parseSeven)

main :: IO ()
main = do
  contents <- readFile "program.seven"
  case (parseSeven contents) of
      Right program ->
          S.eval program >> return ()
      Left err -> putStrLn ("Failed to parse program" ++ show err)
  return ()
