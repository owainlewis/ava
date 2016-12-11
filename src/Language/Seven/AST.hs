module Language.Seven.AST where

import           Data.Monoid ((<>))

data Value = Word String
           | Number Int
           | FList [Value]
           | String String
           | Boolean Bool
           | Variable String Value
           | Procedure String [Value]
           | Comment String
  deriving ( Eq
           , Show
           )

isWord :: Value -> Bool
isWord (Word _) = True
isWord _ = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

isList :: Value -> Bool
isList (FList _) = True
isList _ = False

isString :: Value -> Bool
isString (String _) = True
isString _ = False

isBoolean :: Value -> Bool
isBoolean (Boolean _) = True
isBoolean _ = False

isVariable :: Value -> Bool
isVariable (Variable _ _) = True
isVariable _ = False

isProc :: Value -> Bool
isProc (Procedure _ _) = True
isProc _ = False

-- instance Show Value where
--     show (Number n) = show n
--     show (Word w) = show w
--     show (FList xs) = 
--     show (Procedure k v) = "() " <> k <> " -> [...]"
