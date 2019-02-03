all: build

build:
	dune build

clean:
	git clean -f
	dune clean

test:
	dune runtest -f

doc:
	dune build @doc

benchmark:
	$(MAKE) -C benchmark

.PHONY: build test clean doc benchmark example
