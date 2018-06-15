# BacCaml

This is an experimental meta-hybrid JIT compiler using both approach of method and tracing JIT.

This project is forked from [min-caml](https://github.com/esumii/min-caml), an educational mini ML compiler.

## BacCaml Project

- [BacCaml](https://github.com/prg-titech/baccaml): meta JIT Compiler
- [Armin](https://github.com/prg-titech/armin): interface language for BacCaml

## Usage

- Setup

	For build BacCaml:

	```bash
	$ opam install -y core ppx_deriving ounit
	```

	For your developing environment:

	```bash
	$ opam install -y tuareg merlin ocp-indent utop
	```

- Build

	``` bash
	$ make
	```

- Test

	``` bash
	$ make test
	```
