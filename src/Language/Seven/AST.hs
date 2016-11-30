module Language.Seven.AST
  ( Value(..)
  ) where

import Data.Monoid((<>))


data Value = Word String
           | Number Int
           | Procedure String [Value]
  deriving ( Eq )

instance Show Value where
    show (Number n) = show n
    show (Word w) = show w
    show (Procedure k v) = "() " <> k <> " -> [...]"
