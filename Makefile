all: build

build:
	dune build

clean:
	dune clean
	$(RM) *.so *.s

test:
	dune runtest -f
	$(RM) *.so *.s

doc:
	dune build @doc

benchmark:
	$(MAKE) -C benchmark

.PHONY: build test clean doc benchmark example
