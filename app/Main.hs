{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified          Language.Ava.Intermediate.Reader as Reader
import qualified          Language.Ava.Core as Core

main :: IO ()
main = do
    ava <- Reader.loadAva "language.ava"
    case (ava) of
        Left e -> print e
        Right ins -> Core.execute1 ins >> return ()


