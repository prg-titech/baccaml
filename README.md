[![Build Status](https://travis-ci.org/prg-titech/baccaml.svg?branch=develop)](https://travis-ci.org/prg-titech/baccaml)

# BacCaml: The Meta-Hybrid Just-In-Time Compiler

Meta-interpreter-based just-in-time compiler frameworks (RPython, Truffle/Graal) are useful to build a performant language runtime.
RPython and Truffle/Graal, two most successfully implemented frameworks employ different compilation strategies.
RPython's strategy is based on a tracing, on the other hand Truffle's strategy is based on a method invocation.
The important thing is that those strategies have their own pros and cons.

We propose a _meta-hybrid JIT compilation framework_, and its experimental implementation called BacCaml.
The main features of BacCaml are:

- applying both method- and tracing-based compilation for different program parts
- realiizing a hybrid JIT compiler with a single interpreter definition

This project is forked from <a href="https://github.com/esumii/min-caml">MinCaml</a>.

## Prerequiements

You need the OCaml 4.07.0+ configured with 32bit mode. This is run only on Linux.

## Papers

- MoveVMs'19 workshop: Yusuke Izawa, Hidehiko Masuhara, Tomoyuki Aotani, "Extending a Meta-tracing Compiler to Mix Method and Tracing Compilation". [[url]](https://3tty0n.github.io/baccaml-programming-morevms-2019.pdf)

## Usage

To run the interpreter already defined, execute following commands.

```bash
$ dune build interp/test_interp.exe
$ dune exec interp/test_interp.exe
```
