module Language.Ava.TypeCheck.Composable where

class (Composeable a) where
    comp :: a -> a -> Bool
