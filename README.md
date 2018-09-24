[![Build Status](https://travis-ci.org/prg-titech/baccaml.svg?branch=develop)](https://travis-ci.org/prg-titech/baccaml)

<div align=center>
<img src="etc/image/baccaml.png" width=400>
</div>

---

<div style="text-align: center;">
This is an experimental meta-hybrid JIT compiler using both approach of method and tracing JIT.
This project is forked from <a href="https://github.com/esumii/min-caml">min-caml</a>, an educational mini ML compiler.
</div>

## BacCaml Project

- BacCaml: The Meta JIT Compiler
- RCaml: CLI interface
- Armin: interface language for BacCaml


## Using BacCaml

### Building trace:

```bash
$ dune exec [target].exe -- \
	-file [filename] \
	-type (mjit|tjit) \
	-red [red variables] \
	-green [green variables] \
	-code [bytecode] \
	-o [output]
```

### Building executable:

```bash
$ dune exec src/bin/baccaml_cli.exe -- \
	-type (mjit|tjit) \
	-(dump|emit|build) \
	-trace [trace name] \
	[intepreter name]
```

### Development

- Install dependencies:

	```shell
	$ opam install core menhir ppx_deriving stringext logs fmt dune
	```

- Install IDEs:

	```shell
	$ opam install tuareg merlin ocp-indent utop
	```
