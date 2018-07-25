default: build

all: build test

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31

MAIN = main.exe
ARMIN = armin.exe
COMPILER = min_caml.exe

BUILD_DIR = _build/default

LIBS = core menhir oUnit ppx_deriving logs fmt stringext

.PHONY: build
build:
	jbuilder build
	ln -sf $(BUILD_DIR)/src/mincaml/$(COMPILER) .
	ln -sf $(BUILD_DIR)/src/armin/$(ARMIN) .
	ln -sf $(BUILD_DIR)/src/armin/toplevel.exe .

.PHONY: clean
clean:
	jbuilder clean
	git clean -dfXq
	@rm -rf $(TRASH)

.PHONY: clean-jit
clean-jit:
	rm -rf *.o *.s test/*.o test/*.s *.dSYM
	rm -rf simple*_*j

.PHONY: clean-all
clean-all: clean clean-jit

.PHONY: test
test:
	jbuilder runtest -f

.PHONY: test-one
test-one:
	jbuilder build test/$(SPEC).exe
	cd _build/default/test || exit 1 && ./$(SPEC).exe

.PHONY: indent
indent:
	for f in `find src test -name "*.ml"`; do ocp-indent $$f > tmp.txt && cat tmp.txt > $$f; done && rm -f tmp.txt

.PHONY: setup
setup:
	opam install $(LIBS)

-include Makefile.mincaml
