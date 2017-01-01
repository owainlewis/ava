# AVA

A stack based programming language with AOT checking.

Ava is based heavily on the Joy programming language but has explict support for quotations. Ava programs are stack based. Quotations are meta programs that themselves hav eyet to be evaluated.

1 2 3 == { 1 2 3 } apply

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
