{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified          Language.Ava.Intermediate.Reader as Reader
import qualified          Language.Ava.Core as Core

bipure :: (Applicative f, Applicative g) => a -> f (g a)
bipure = pure . pure

main :: IO ()
main = let debug = True in
  do
    ava <- Reader.loadAva "language.ava"
    case (ava) of
        Left e -> print e
        Right ins -> Core.execute1 ins >>=
          (if debug then print else bipure ()) >> return ()



