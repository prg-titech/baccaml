all: build

build:
	dune build

clean: comp-clean
	dune clean

comp-clean:
	$(RM) *.so *.s

test:
	dune runtest -f
	$(RM) *.so *.s

benchmark:
	$(MAKE) -C benchmark

.PHONY: build test clean doc benchmark setup
