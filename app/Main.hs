{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main ) where

import qualified          Language.Ava.Reader as Reader
import qualified          Language.Ava.Compliler as Compiler

runFile :: FilePath -> IO ()
runFile path = (either print runInstructions) =<< Reader.loadAva path
    where runInstructions instrs = Core.executeWithStdLib instrs >>= print >> return ()

main :: IO ()
main = runFile "language.ava"
