[![Build Status](https://travis-ci.org/prg-titech/baccaml.svg?branch=develop)](https://travis-ci.org/prg-titech/baccaml)

<div align=center>
<img src="etc/image/baccaml.png" width=400>
</div>

---

This is an experimental meta-hybrid JIT compiler using both approach of method and tracing JIT.
This project is forked from <a href="https://github.com/esumii/min-caml">MinCaml</a>.

## Usage

```bash
$ dune exec baccaml -- -file [interpreter].ml [file.arg]
$ dune exec src/bin/baccaml_builder.exe -- [interpreter].ml \
	-type (tjit|mjit) -trace [name of trace] -o [output] -build
```

### Example

```bash
$ dune exec baccaml -- -file benchmark/baccaml/test_interp_tj.ml \
	benchmark/armin/fib.am benchmark/armin/fib_26.am benchmark/armin/fib_18.am
$ dune exec src/bin/baccaml_builder.exe -- benchmark/baccaml/test_interp_tj.ml \
	-type tjit -trace fib -trace fib_26 -o test_fib_tj -build
```

## Development

- Install dependencies:

	```shell
	$ opam install core menhir ppx_deriving stringext logs fmt dune
	```

- Install IDEs:

	```shell
	$ opam install tuareg merlin ocp-indent utop
	```
