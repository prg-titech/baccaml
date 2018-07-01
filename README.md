[![Build Status](https://travis-ci.org/prg-titech/baccaml.svg?branch=develop)](https://travis-ci.org/prg-titech/baccaml)

<div align=center>
<img src="image/baccaml.png" width=400>
</div>

---

<div style="text-align: center;">
This is an experimental meta-hybrid JIT compiler using both approach of method and tracing JIT.
This project is forked from <a href="https://github.com/esumii/min-caml">min-caml</a>, an educational mini ML compiler.
</div>

## BacCaml Project

- [BacCaml](https://github.com/prg-titech/baccaml): meta JIT compiler
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
