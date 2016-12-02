module Language.Seven.AST where

import Data.Monoid((<>))


data Value = Word String
           | Number Int
           | Procedure String [Value]
  deriving ( Eq, Show )

-- instance Show Value where
--     show (Number n) = show n
--     show (Word w) = show w
--     show (Procedure k v) = "() " <> k <> " -> [...]"
