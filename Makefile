PACKS = merlin tuareg utop dune oUnit ppx_deriving

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

setup:
	opam update
	opam install -y ${PACKS}

.PHONY: build test clean doc benchmark setup
