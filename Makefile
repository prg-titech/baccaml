default: compiler interp

.all: compiler interp clean example test

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31
OCAMLBUILD_OPTIONS = -use-ocamlfind
COMPILER = min-caml
INTERPRETER = min-camli

TESTCASES = jit_test interp_test pypy_fig3_test \
simple1_test simple2_test simple3_test

EXAMPLES = print sum-tail gcd sum fib ack even-odd adder \
funcomp cls-rec cls-bug cls-bug2 cls-reg-bug shuffle \
spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 inprod inprod-rec \
inprod-loop matmul matmul-flat manyargs fib-tail array array2 \
float tuple

.PHONY: compiler
compiler:
	@ocamlbuild $(OCAMLBUILD_OPTIONS) src/main.byte
	@mv main.byte min-caml

.PHONY: interp
interp:
	@ocamlbuild $(OCAMLBUILD_OPTIONS) src/jit/min-camli.byte
	@mv min-camli.byte min-camli

.PHONY: clean
clean:
	@rm -f $(TRASH)
	ocamlbuild -clean

.PHONY: jit_clean
jit_clean:
	@rm -f *.o *.s **/*.o **/*.s

.PHONY: test
test:
	@for case in $(TESTCASES); do \
		echo "================================================="; \
		echo $$case; \
		echo "================================================="; \
		$(MAKE) jit_clean; \
		ocamlbuild $(OCAMLBUILD_OPTIONS) test/$$case.byte || exit 1; \
		$(MAKE) jit_clean; \
		./$$case.byte; \
	done

.PHONY: example
example: $(EXAMPLES:%=example/%.cmp)

.PRECIOUS: example/%.s example/% example/%.res example/%.ans example/%.cmp
TRASH = $(EXAMPLES:%=example/%.s) $(EXAMPLES:%=example/%) $(EXAMPLES:%=example/%.res) $(EXAMPLES:%=example/%.ans) $(EXAMPLES:%=example/%.cmp)

example/%.s: compiler example/%.ml
	./$(COMPILER) example/$*
example/%: example/%.s lib/libmincaml.S lib/stub.c
	$(CC) $(CFLAGS) -m32 $^ -lm -o $@
example/%.res: example/%
	$< > $@
example/%.ans: example/%.ml
	ocaml $< > $@
example/%.cmp: example/%.res example/%.ans
	diff $^ > $@
