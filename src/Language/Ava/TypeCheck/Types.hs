module Language.Ava.TypeCheck.Types where

import Language.Ava.Intermediate.Instruction
import Language.Ava.Base.AST

data Arity = Arity {
    takes :: Int
  , leaves :: Int
} deriving (Eq, Ord, Show)

data TypeKind = TAny
              | TInteger
              | TFloat
              | TString
              | TSeq
              deriving ( Eq, Show )

data TypePattern = TypePattern {
    inputs :: [TypeKind]
  , outputs :: [TypeKind]
} deriving (Eq, Show )

tp :: [TypeKind] -> [TypeKind] -> TypePattern
tp inputs outputs = TypePattern { inputs = inputs
                                , outputs = outputs
                                }

-- | Given an instruction, what is it's arity?
--
-- typePattern (TPush v) = 0
-- typePattern (TPop) = 1
-- typePattern (TApply o) = 0
-- typePattern (TDefine o vs) = 0
-- typePattern (TLet o v) = 0
-- typePattern (TDup) = 1
typePattern (TSwap) = tp [TAny, TAny] [TAny, TAny]
-- typePattern (TCons) = 2
-- typePattern (TUncons) = 1
-- typePattern (TChoice) = 0
-- typePattern (TStack) = 0
-- typePattern (TUnstack) = 0
-- typePattern (TInfra) = 0
-- typePattern (TMult) = 0
typePattern (TAdd) = tp [TInteger, TInteger] [TInteger]
-- typePattern (TSub) = 0
-- typePattern (TDiv) = 0
-- typePattern (TGt) = 0
-- typePattern (TLt) = 0
-- typePattern (TEq) = 0
-- typePattern (TDot) = 0
-- typePattern (TPrint) = 0

