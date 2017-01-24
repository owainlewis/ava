module Language.Ava.Simplify where

-- The process of simplification here applies to procedures.
--
-- It involes taking a procedure that contains many user defined
-- words and re-writing in terms of the lower level instruction forms
--
-- Consider the following two definitions
--
-- square = { dup * } => { TDup TMult }
--
-- bisquare = { square square } => ??
--
-- The first procedure cannot be reduced since it already evaluates to two instructions
--
-- The second procedure can be rewritten in terms of simple instructions via substitution.
--
-- After simplification we can rewrite bisquare as
--
-- bisquare = { TDup TMult TDup TMult }
--



