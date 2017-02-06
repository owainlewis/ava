module Language.Ava.Base.Error
    ( failGeneric
    , failState
    , ProgramError(..)
    ) where

import Data.Semigroup((<>))

-------------------------------------------------------------

type Actual = String
type Expected = String

-------------------------------------------------------------

data ProgramError =
                    InvalidState String
                  | GenericError String
                  | TypeError Actual Expected

-------------------------------------------------------------

instance Show ProgramError where
  show (InvalidState op) = "Invalid state for operation " <> op
  show (GenericError e)  = e

-------------------------------------------------------------

failGeneric :: Monad m => String -> m (Either ProgramError a)
failGeneric e = return . Left $ GenericError e

failState :: Monad m => String -> m (Either ProgramError b)
failState e = return . Left $ InvalidState e
