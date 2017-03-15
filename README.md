# AVA

Ava is a programming language exploring the idea of programs that can mutate and write themselves. 

It is a stack based programming language, heavily inspired by Joy.

Ava programs are just a list of instructions. Ava supports the idea of quotations (meta programs) as a first class language construct. 

Quotations are themselves Ava programs that can be evalulated. Unlike Joy, Ava quotations are distinct from lists

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

## DOCS

Native operations

* POP
* SWAP
* DUP
* CONS
* UNCONS
* CHOICE
* STACK
* UNSTACK
* INFRA
