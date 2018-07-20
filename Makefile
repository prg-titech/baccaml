.default: build

all: build

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31

MAIN = main.exe
ARMIN = armin.exe
COMPILER = min_caml.exe

BUILD_DIR = _build/default

LIBS = core oUnit ppx_deriving logs fmt sequence stringext

.PHONY: build
build:
	jbuilder build
	ln -sf $(BUILD_DIR)/src/mincaml/$(COMPILER) .
	ln -sf $(BUILD_DIR)/src/armin/$(ARMIN) .
	ln -sf $(BUILD_DIR)/src/armin/toplevel.exe .

.PHONY: setup
setup:
	opam install $(LIBS)

.PHONY: clean
clean:
	jbuilder clean
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

EXAMPLES = print sum-tail gcd sum fib ack even-odd adder funcomp cls-rec cls-bug \
					 cls-bug2 cls-reg-bug shuffle spill spill2 spill3 join-stack join-stack2 \
					 join-stack3 join-reg join-reg2 non-tail-if2 inprod inprod-rec inprod-loop \
					 matmul matmul-flat manyargs fib-tail array array2 float tuple

.PHONY: example
example: $(EXAMPLES:%=example/%.cmp)

.PRECIOUS: example/%.s example/% example/%.res example/%.ans example/%.cmp
TRASH = $(EXAMPLES:%=example/%.s) $(EXAMPLES:%=example/%) $(EXAMPLES:%=example/%.res) $(EXAMPLES:%=example/%.ans) $(EXAMPLES:%=example/%.cmp)

example/%.s: example/%.ml
	./$(COMPILER) example/$*.ml
example/%: example/%.s lib/libmincaml.s lib/stub.c
	$(CC) $(CFLAGS) -m32 $^ -o $@
example/%.res: example/%
	$< > $@
example/%.ans: example/%.ml
	ocaml $< > $@
example/%.cmp: example/%.res example/%.ans
	diff $^ > $@
