all: build

build:
	dune build

clean: comp-clean
	dune clean

comp-clean:
	$(RM) -r _static
	$(RM) *.so *.s *.o

test: comp-clean
	dune runtest -f

benchmark:
	$(MAKE) -C benchmark

.PHONY: build test clean doc benchmark setup
