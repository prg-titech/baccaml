CC 		= gcc
CFLAGS		= -g -m32
OCAMLLDFLAGS 	= -warn-error -31

EXAMPLES 	= print sum-tail gcd sum fib ack even-odd adder funcomp cls-rec cls-bug \
		cls-bug2 cls-reg-bug shuffle spill spill2 spill3 join-stack join-stack2 \
		join-stack3 join-reg join-reg2 non-tail-if2 inprod inprod-rec inprod-loop \
		matmul matmul-flat manyargs fib-tail array array2 float tuple

TRASH 		= $(EXAMPLES:%=etc/example/%.s) \
		$(EXAMPLES:%=etc/example/%) \
		$(EXAMPLES:%=etc/example/%.res) \
		$(EXAMPLES:%=etc/example/%.ans) \
		$(EXAMPLES:%=etc/example/%.cmp) \
		*.o *.s **/*.dSYM

default: build

all: build test

build:
	dune build

clean: git-clean
	dune clean
	rm -rf $(TRASH)

git-clean:
	git clean -f

test:
	dune runtest -f

doc:
	dune build @doc

benchmark:
	$(MAKE) -C benchmark

example: $(EXAMPLES:%=etc/example/%.cmp)

.PRECIOUS: etc/example/%.s etc/example/% etc/example/%.res etc/example/%.ans etc/example/%.cmp

etc/example/%.s: etc/example/%.ml
	dune exec src/base/$(COMPILER) etc/example/$*.ml

etc/example/%: etc/example/%.s lib/libmincaml.S lib/stub.c
	$(CC) $(CFLAGS) -m32 $^ -o $@

etc/example/%.res: etc/example/%
	$< > $@

etc/example/%.ans: etc/example/%.ml
	ocaml $< > $@

etc/example/%.cmp: etc/example/%.res etc/example/%.ans
	diff $^ > $@

.PHONY: build test clean clean-dSYM doc benchmark example
