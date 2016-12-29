# AVA

A stack based programming language with AOT checking

```factor
MODULE Std.Ava.Example

USING Ava.Core.IO as io

function double (x -> x) @doc "Doubles a number on the stack" {
  2 flip pop
  dup cons unit
}

let x = 20
let y = 40

if { 20 double 40 = }
  "Hello, World!" io::print
end

if { 20 double 40 = }
  "Hello, World!" io::print
else
  "Foobar!" io::print
end

let data = {:foo 10, :bar 20}

data map::keys print

x y double io::print
```

## DOCS

Standard Library

* AND Push the result of the logical AND of the top two items.
* OR Push the result of the logical OR of the top two items.
* NOT Push the result of the logical NOT of the top item.
* NAND Push the result of the logical NAND of the top two items.
* NOR Push the result of the logical NOR of the top two items

```forth
# =====================================
# Example program
# =====================================

@define DOUBLE (x -> x) {
  2 TIMES
}

10 20 DOUBLE SWAP PRINT
```

## Features

* Functional Combinators (Joy)
* Stack language
* Type system
* AOT Checking

# Goals

* A powerful language with a simple conceptual model (ala LISP)
* Practical (good support for IO operations on files, sockets, HTTP etc)
* Implement a type checking algorithm

# Theory

### Type Checking

Seven aims to introduce a type system to a forth/joy like stack language.

### Language Primatives

* Blocks { }
* Lists [ ]
* Strings ""
* Numbers
* Procedures (define foo ( ) @doc { ... })
* Variables { let x = 10 }
