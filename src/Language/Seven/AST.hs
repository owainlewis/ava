module Language.Seven.AST where

import           Data.Monoid ((<>))

data Value = Word String
           | Number Int
           | Vector [Value]
           | String String
           | Boolean Bool
           | Variable String Value
           | Procedure String [Value]
           | Comment String
  deriving ( Show )

instance Eq Value where
  (Word x)    == (Word y)    = x == y
  (Number x)  == (Number y)  = x == y
  (String x)  == (String y)  = x == y
  (Boolean x) == (Boolean y) = x == y
  (Vector xs) == (Vector ys) = xs == ys
  (Procedure x1 y1) ==
    (Procedure x2 y2) = x1 == x2 && y1 == y2

instance Ord Value where
  (Word x) `compare` (Word y) = x `compare` y
  (Number x) `compare` (Number y) = x `compare` y

isWord :: Value -> Bool
isWord (Word _) = True
isWord _ = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

isVector :: Value -> Bool
isVector (Vector _) = True
isVector _ = False

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
