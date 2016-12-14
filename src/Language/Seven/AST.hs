module Language.Seven.AST where

import           Data.Monoid ((<>))

data Value = Word String
           | Integer Int
           | Vector [Value]
           | String String
           | Boolean Bool
           | Variable String Value
           | Procedure String [Value]
           | Comment String -- TODO remove me?

instance Show Value where
    show (Word x) = x
    show (Integer x) = show x
    show (Vector x) = "[ ... ]"
    show (String x) = x
    show (Boolean x) = show x
    show (Variable x y) = "VAR " ++ x
    show (Procedure x y) = "() =>"
    show (Comment x) = x
    
instance Eq Value where
  (Word x)    == (Word y)    = x == y
  (Integer x)  == (Integer y)  = x == y
  (String x)  == (String y)  = x == y
  (Boolean x) == (Boolean y) = x == y
  (Vector xs) == (Vector ys) = xs == ys
  (Procedure x1 y1) ==
    (Procedure x2 y2) = x1 == x2 && y1 == y2

instance Ord Value where
  (Word x) `compare` (Word y) = x `compare` y
  (Integer x) `compare` (Integer y) = x `compare` y

isWord :: Value -> Bool
isWord (Word _) = True
isWord _ = False

isInteger :: Value -> Bool
isInteger (Integer _) = True
isInteger _ = False

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
