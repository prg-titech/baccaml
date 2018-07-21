[![Build Status](https://travis-ci.org/prg-titech/baccaml.svg?branch=develop)](https://travis-ci.org/prg-titech/baccaml)

<div align=center>
<img src="image/baccaml.png" width=300>
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

## Usage

- Setup

	Install dependencies:

	```bash
	$ make setup
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
