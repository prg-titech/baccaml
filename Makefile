CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31

ARMIN = armin.exe
ARMIN_TOP = toplevel.exe
COMPILER = min_caml.exe

BUILD_DIR = _build/default

default: build

all: build test

.PHONY: build
build:
	dune build
	dune build src/armin/$(ARMIN)
	dune build src/armin/$(ARMIN_TOP)
	ln -sf $(BUILD_DIR)/src/mincaml/$(COMPILER) .
	ln -sf $(BUILD_DIR)/src/armin/$(ARMIN) .
	ln -sf $(BUILD_DIR)/src/armin/$(ARMIN_TOP) .

.PHONY: clean
clean:
	dune clean
	git clean -dfXq
	rm -rf $(TRASH)

.PHONY: clean-jit
clean-jit:
	rm -rf *.o *.s test/*.o test/*.s *.dSYM
	rm -rf simple*_*j

.PHONY: clean-all
clean-all: clean clean-jit

.PHONY: test
test:
	dune runtest -f

.PHONY: indent
indent:
	for f in `find src test -name "*.ml"`; do ocp-indent $$f > tmp.txt && cat tmp.txt > $$f; done && rm -f tmp.txt

EXAMPLES = \
	print sum-tail gcd sum fib ack even-odd adder funcomp cls-rec cls-bug \
	cls-bug2 cls-reg-bug shuffle spill spill2 spill3 join-stack join-stack2 \
	join-stack3 join-reg join-reg2 non-tail-if2 inprod inprod-rec inprod-loop \
	matmul matmul-flat manyargs fib-tail array array2 float tuple

.PHONY: example
example: $(EXAMPLES:%=example/%.cmp)

.PRECIOUS: example/%.s example/% example/%.res example/%.ans example/%.cmp

TRASH = \
      $(EXAMPLES:%=example/%.s) \
      $(EXAMPLES:%=example/%) \
      $(EXAMPLES:%=example/%.res) \
      $(EXAMPLES:%=example/%.ans) \
      $(EXAMPLES:%=example/%.cmp)

example/%.s: example/%.ml
	./$(COMPILER) example/$*.ml
example/%: example/%.s lib/libmincaml.S lib/stub.c
	$(CC) $(CFLAGS) -m32 $^ -o $@
example/%.res: example/%
	$< > $@
example/%.ans: example/%.ml
	ocaml $< > $@
example/%.cmp: example/%.res example/%.ans
	diff $^ > $@
