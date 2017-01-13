{-# LANGUAGE TemplateHaskell #-}
module Language.Ava.Interp
  ( )
  where

import           Control.Lens
import           Control.Monad    (foldM, join)
import           Data.Bifunctor
import qualified Data.Map         as M
import qualified Language.Ava.AST as AST

data Stack = Stack {
    _runtime    :: [AST.Value]
  , _procedures :: M.Map String [AST.Value]
  , _vars       :: M.Map String (AST.Value)
} deriving ( Eq, Show )

makeLenses ''Stack

data ProgramError = StateError

type Operation = Stack -> AST.Value-> Either ProgramError (IO Stack)

data Result a b = Success a | Failure b

instance Bifunctor Result where
  bimap f _ (Success a) = Success (f a)
  bimap _ g (Failure b) = Failure (g b)

modify :: ([AST.Value] -> [AST.Value]) -> Stack -> Stack
modify f (Stack x y z) = Stack (f x) y z

push :: Stack -> AST.Value-> IO (Either ProgramError Stack)
push (Stack x y z) value = pure . Right $ Stack (value : x) y z

pop :: Stack -> IO Stack
pop (Stack (x:xs) y z) = pure $ Stack xs y z
pop (Stack x y z)      = pure $ Stack x y z

dup (Stack (x:xs) y z) = Right . pure $ Stack (x:x:xs) y z

chain f (Right outcome) = Right (f outcome)
chain f (Left e)        = Left e

-- eval :: Stack ->
--         AST.Value ->
--         Either ProgramError (IO Stack)
-- eval stack (AST.Integer x) = push stack (AST.Integer x)
-- eval stack (AST.Float x)   = push stack (AST.Float x)
-- eval stack (AST.String x)  = push stack (AST.String x)

-- liftedF :: (IO Stack) -> AST.Value -> Either ProgramError (IO Stack)
-- liftedF ios v = do
--   outcome <- ios >>= (\s -> return $ eval s v)
--   return outcome

-- interp :: Either ProgramError (IO Stack) ->
--           [AST.Value] ->
--           Either ProgramError (IO Stack)
-- interp (Left e) _ = (Left e)
-- --interp (Right ios) (next:intrs) = do
-- --         outcome <- ios
-- --         Left StateError
-- --    Right ios -> ios >>= (\s -> eval stack next)

-- -- A program is just a list of operations that runs over some stack state


