# AVA

Ava is a programming language exploring the idea of programs that can mutate and write themselves. 

It is a stack based programming language, heavily inspired by Joy.

Ava programs are just a list of instructions. Ava supports the idea of quotations (meta programs) as a first class language construct. 

Quotations are themselves Ava programs that can be evalulated. Unlike Joy, Ava quotations are distinct from lists.

## Example

An Ava program is a sequence of instructions

> Program = [ Instruction ]

A program may contain and generate other programs via quotations

> Program A = [ { 1 2 3 } apply ] 
> Program B = [ 1 2 3 ]
> ProgramA == ProgramB

### Functions

Ava functions are just a list of instructions bound to an identifier

```factor
define square = [ dup * ]
```

### Variables

Variables are immutable

```factor
let x = 10

let y = 20
```

### Combinators

#### swap
```
a b swap => b a
```

#### i

The i combinator expects a quotation at the top of the stack.

```
{ P } i => P.
```

#### x

The x combinator

```
[P] x => [P] P
```

#### b

#### infra

```
L [M] [P] infra => L [N]
```

The infra combinator temporarily discards the current stack and takes M as the
current stack state. It then executes the top quotation P which returns a new
stack N. The new stack N is pushed back as a quotation onto the original stack

#### times

```
0 [P] times = id.
n [P] times = P (n-1) [P] times
```

#### branch

```
true  { P } { Q } branch => P.
false { P } { Q } branch => Q.
```

# Instructions 

The AVA language is made up of two layers. The lowest layer is a sequence of instructions that manipulate or modify a stack.

The lower level machine / VM instructions are

```
TPush
TPop
TDup
TSwap
TCons
TUncons
TChoice
TApply
TLet
TDefine
TStack
TUnstack
TInfra
TMult
TAdd
TSub
TDiv
TGt
TLt
TEq
TDot
TPrint
TNoop
```
