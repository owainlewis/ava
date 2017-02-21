module Language.Ava.TypeCheck.Types
    ( TypePattern
    , typePattern
    ) where

import Language.Ava.Intermediate.Instruction
import Language.Ava.Base.AST
import Language.Ava.Internal.Stack(Stack)

data TypeKind = TAny
              | TInteger
              | TFloat
              | TString
              | TBoolean
              | TSeq
              | TVoid
              deriving ( Eq, Show )

data TypePattern = TypePattern {
    inputs  :: [TypeKind]
  , outputs :: [TypeKind]
} deriving (Eq, Show)

tp :: [TypeKind] -> [TypeKind] -> TypePattern
tp inputs outputs = TypePattern { inputs = inputs
                                , outputs = outputs
                                }

-- | Given an instruction, what is it's arity?
--
typePattern (TPush v) = error "TODO"
typePattern (TPop) = error "TODO"
typePattern (TApply o) = error "TODO"
typePattern (TDefine o vs) = error "TODO"
typePattern (TLet o v) = error "TODO"
typePattern (TDup) = tp [TAny] [TAny, TAny]

typePattern (TSwap) = tp [TAny, TAny] [TAny, TAny]

typePattern (TCons) = error "TODO"
typePattern (TUncons) = error "TODO"
typePattern (TChoice) = error "TODO"
typePattern (TStack) = error "TODO"
typePattern (TUnstack) = error "TODO"
typePattern (TInfra) = error "TODO"

-- | Numeric binary operations have a simpe type
typePattern (TMult) = tp [TInteger, TInteger] [TInteger]
typePattern (TAdd) = tp [TInteger, TInteger] [TInteger]
typePattern (TSub) = tp [TInteger, TInteger] [TInteger]

typePattern (TDiv) = error "TODO"

-- Binary boolean operations have a simple type
typePattern (TGt) = tp [TInteger, TInteger] [TBoolean]
typePattern (TLt) = tp [TInteger, TInteger] [TBoolean]
typePattern (TEq) = tp [TInteger, TInteger] [TBoolean]

-- IO or `void` types

typePattern (TDot) = tp [] [TVoid]
typePattern (TPrint) = tp [TAny] [TVoid]

-- Do type patterns compose ?

-- Given X Y where X and Y are both type patterns, can we combine them successfully?

class (Composeable a) where
    comp :: a -> a -> Bool

-- This is not strictly true. There are many operations where the
-- current state of the stack reflects whether two items are composable
-- but just sketching out some ideas for now
instance (Composeable Instruction) where
    comp a b =
      let tp1 = typePattern a
          tp2 = typePattern b
          in
          outputs tp1 == inputs tp2

-- What we really what here is a fold step that opperates in the world of
-- type patterns
