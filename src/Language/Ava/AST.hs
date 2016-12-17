{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.AST where

import           Data.List   (intersperse)
import           Data.Monoid ((<>))

data Value = Word! String
           | Integer! Int
           | Float! Double
           | Vector! [Value]
           | String! String
           | Boolean! Bool
           | Variable! String Value
           | Procedure! String [Value]

instance Show Value where
    show (Word x)        = x
    show (Integer x)     = show x
    show (Float x)      = show x
    show (Vector xs)     = let innerForms =
                                 concat $ intersperse "," (map show xs) in
                          "[" ++ innerForms ++ "]"
    show (String x)      = x
    show (Boolean x)     = show x
    show (Variable x y)  = "VAR " ++ x
    show (Procedure x y) = "() =>"

instance Eq Value where
  (Word x)          == (Word y)          = x == y
  (Integer x)       == (Integer y)       = x == y
  (Float x)        == (Float y)        = x == y
  (Vector xs)       == (Vector ys)       = xs == ys
  (String x)        == (String y)        = x == y
  (Boolean x)       == (Boolean y)       = x == y
  (Variable x1 y1)  == (Variable x2 y2)  = x1 == x2 && y1 == y2
  (Procedure x1 y1) == (Procedure x2 y2) = x1 == x2 && y1 == y2
  x                 == y                 = False

instance Ord Value where
  (Word x)          `compare` (Word y) = x `compare` y
  (Integer x)       `compare` (Integer y) = x `compare` y
  (Float x)        `compare` (Float y) = x `compare` y
  (Vector xs)       `compare` (Vector ys)       = xs `compare` ys
  (String x)        `compare` (String y)        = x `compare` y
  (Boolean x)       `compare` (Boolean y)       = x `compare` y
  (Variable x1 y1)  `compare` (Variable x2 y2)  = x1 `compare` x2
  (Procedure x1 y1) `compare` (Procedure x2 y2) = x1 `compare` x2

isWord :: Value -> Bool
isWord (Word _) = True
isWord _        = False

isInteger :: Value -> Bool
isInteger (Integer _) = True
isInteger _           = False

isFloat :: Value -> Bool
isFloat (Float _) = True
isFloat _         = False

isVector :: Value -> Bool
isVector (Vector _) = True
isVector _          = False

isString :: Value -> Bool
isString (String _) = True
isString _          = False

isBoolean :: Value -> Bool
isBoolean (Boolean _) = True
isBoolean _           = False

isVariable :: Value -> Bool
isVariable (Variable _ _) = True
isVariable _              = False

isProc :: Value -> Bool
isProc (Procedure _ _) = True
isProc _               = False
