{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main ) where

import qualified          Language.Ava.Intermediate.Reader as Reader
import qualified          Language.Ava.Core as Core

runFile :: FilePath -> IO ()
runFile path = (either print runInstructions) =<< Reader.loadAva path
    where runInstructions instrs = Core.execute1 instrs >>= print >> return ()

main :: IO ()
main = runFile "language.ava"
