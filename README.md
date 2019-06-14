[![Build Status](https://travis-ci.org/prg-titech/baccaml.svg?branch=develop)](https://travis-ci.org/prg-titech/baccaml)

# BacCaml: The Meta-Hybrid Just-In-Time Compiler

Meta-interpreter-based just-in-time compiler frameworks (RPython, Truffle/Graal) are useful to build a performant language runtime.
RPython and Truffle/Graal, two most successfully implemented frameworks employ different compilation strategies.
RPython's strategy is based on a tracing, on the other hand Truffle's strategy is based on a method invocation.
The important thing is that those strategies have their own pros and cons.

We propose a _meta-hybrid JIT compilation framework_, and its' experimental implementation called BacCaml.
The three main features of BacCaml are:

- applying both method- and tracing-based compilation for different program parts
- realiizing a hybrid JIT compiler with a single interpreter definition
- sharing many parts of implementation between method-based and tracing-based compilation


This project is forked from <a href="https://github.com/esumii/min-caml">MinCaml</a>.

## Prerequiements

You need the OCaml 4.07.0+ configured with 32bit mode.

## Usage

To run the interpreter already defined, execute following commands.

```bash
$ dune build interp/test_interp.exe
$ dune exec interp/test_interp.exe
```

## Debugging

This function is used for ahead-of-time compilation and debugging generated traces.

```bash
$ dune exec baccaml -- -file [interpreter].ml [file.arg]
$ dune exec src/bin/baccaml_builder.exe -- [interpreter].ml \
	-type (tjit|mjit) -trace [name of trace] -o [output] -build

# example
$ dune exec baccaml -- -file benchmark/baccaml/test_interp_tj.ml \
    benchmark/armin/fib.am benchmark/armin/fib_26.am benchmark/armin/fib_18.am
$ dune exec src/bin/baccaml_builder.exe -- benchmark/baccaml/test_interp_tj.ml \
    -type tjit -trace fib -trace fib_26 -o test_fib_tj -build
```
