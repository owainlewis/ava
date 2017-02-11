module FunctorExamples where

import Prelude hiding (fmap)

-----------------------------------------------------

class CovariantFunctor f where
    fmap :: (a -> b) -> f a -> f b

-----------------------------------------------------

class ContravariantFunctor f where
    contramap :: (b -> a) -> f a -> f b

-----------------------------------------------------

class Bifunctor f where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

class Trifunctor f where
    trimap :: (a -> d) -> (b -> e) -> (c -> g) -> f a b c -> f d e g

-----------------------------------------------------

class Profunctor f where
    dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

-----------------------------------------------------

newtype Const a b = Const a

instance ContravariantFunctor (Const a) where
    contramap _ (Const a) = Const a

-----------------------------------------------------
instance CovariantFunctor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)

instance CovariantFunctor [] where
    fmap f [] = []
    fmap f (x:xs) = f x : fmap f xs

instance CovariantFunctor IO where
    fmap f x = x >>= return . f
