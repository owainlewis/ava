# Language Reference

A guide to the Ava combinators

## swap

a b swap => b a

## i

The i combinator expects a quotation at the top of the stack.

{ P } i => P.

## x

The x combinator

[P] x => [P] P

## b

## infra

L [M] [P] infra => L [N]

The infra combinator temporarily discards the current stack and takes M as the
current stack state. It then executes the top quotation P which returns a new
stack N. The new stack N is pushed back as a quotation onto the original stack

## times

0 [P] times = id.
n [P] times = P (n-1) [P] times

## branch

true  { P } { Q } branch => P.
false { P } { Q } branch => Q.
