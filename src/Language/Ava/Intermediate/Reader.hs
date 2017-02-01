module Language.Ava.Intermediate.Reader where

import Language.Ava.Base.Parser(AvaParseError)
import Language.Ava.Intermediate.Instruction(Instruction)
import qualified Language.Ava.Base.Reader as B
import qualified Language.Ava.Intermediate.Transform as Trans

readString :: String -> Either AvaParseError [Instruction]
readString s = fmap (map Trans.eval) (B.readString s)



