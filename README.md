# 7

A stack based programming language with AOT checking

### IDEAS

Macro to swap operation ordering for more familiar syntax. Take this if statement

```
y ELSE x THEN [] IF

;; Becomes

@reverse

IF []
  THEN x
  ELSE y
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

* Lists
* Strings
* Numbers
* Procedures
* Variables
