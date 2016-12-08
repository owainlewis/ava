# 7

A stack based programming language with AOT checking

```forth
# =====================================
# Example program
# =====================================

@define double (x -> x) {
  2 *
}

10 20 double

swap print

40 50 double print swap print
```

# Goals

* A powerful language with a simple conceptual model (ala LISP)
* Practical (good support for IO operations on files, sockets, HTTP etc)
* Implement a type checking algorithm

# Theory

### Language Primatives

* Lists
* Strings
* Numbers
* Procedures
* Variables
