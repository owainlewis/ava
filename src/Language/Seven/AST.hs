module Language.Seven.AST where

import           Data.Monoid ((<>))

data Value = Word String
           | Number Int
           | List [Value]
           | String String
           | Boolean Bool
           | Variable String Value
           | Procedure String [Value]
           | Comment String
  deriving ( Eq, Show )

-- instance Show Value where
--     show (Number n) = show n
--     show (Word w) = show w
--     show (Procedure k v) = "() " <> k <> " -> [...]"
