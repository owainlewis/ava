{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Language.Ava.Compiler as Compiler
import qualified Language.Ava.Reader as Reader

import Control.Monad (forM_)

import qualified Data.Text.IO as TIO

-- | Run a single file and execute it including the STD lib context loaded from disk
--
runFile :: FilePath -> IO ()
runFile path = (either print runInstructions) =<< Reader.loadAva path
  where
    runInstructions instrs =
      Compiler.executeWithStdLib instrs >>= print >> return ()

-- | An interactive REPL environment for the Ava programming language
--
repl :: IO ()
repl =
  let welcome =
        [ "******************"
        , "The AVA programming language"
        , "Press :q to quit"
        , "******************"
        , ""
        ]
  in do forM_ welcome putStrLn
        initStack <- Compiler.go =<< TIO.readFile "lib/language.ava"
        case initStack of
          Left e -> print e >> return ()
          Right s -> goLoop s
  where
    goLoop s = do
      line <- TIO.getLine
      if line == ":q"
        then print "Bye" >> return ()
        else do
          eval <- Compiler.goWithStack s line
          case eval of
            Left e -> print e >> return ()
            Right s2 -> goLoop s2
          return ()

main :: IO ()
main = runFile "lib/language.ava"
