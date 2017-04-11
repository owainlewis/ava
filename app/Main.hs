{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main ) where

import qualified          Language.Ava.Reader as Reader
import qualified          Language.Ava.Compiler as Compiler

import Control.Monad(forM_)

import qualified Data.Text.IO as TIO

runFile :: FilePath -> IO ()
runFile path = (either print runInstructions) =<< Reader.loadAva path
    where runInstructions instrs = Compiler.executeWithStdLib instrs >>= print >> return ()

repl :: IO ()
repl = let welcome = [ "******************"
                     , "AVA"
                     , "Press :q to quit"
                     , "******************"
                     , ""
                     ] in
         do
    forM_ welcome putStrLn
    initStack <- Compiler.go =<< TIO.readFile "lib/language.ava"
    case initStack of
      Left e -> print e >> return ()
      Right s -> goLoop s
    where goLoop s = do
          line <- TIO.getLine
          if line == ":q" then
                          print "Bye" >> return () else do
          eval <- Compiler.goWithStack s line
          case eval of
            Left e -> print e >> return ()
            Right s2 ->
                goLoop s2
          return ()

main :: IO ()
main = runFile "lib/language.ava"
