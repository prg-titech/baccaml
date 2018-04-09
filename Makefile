.default: build

.all: build clean example testall runtest gcc

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31
OCAMLBUILD_OPTIONS = -use-ocamlfind
COMPILER = min-caml

TESTCASES = jit_test interp_test pypy_fig3_test \
simple1_test simple2_test simple3_test

EXAMPLES = print sum-tail gcd sum fib ack even-odd adder \
funcomp cls-rec cls-bug cls-bug2 cls-reg-bug shuffle \
spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 inprod inprod-rec \
inprod-loop matmul matmul-flat manyargs fib-tail array array2 \
float tuple

.PHONY: build
build:
	@ocamlbuild $(OCAMLBUILD_OPTIONS) src/main.byte
	@mv main.byte min-caml

.PHONY: clean
clean:
	@rm -f $(TRASH)
	ocamlbuild -clean

.PHONY: jit_clean
jit_clean:
	@rm -f *.o *.s test/*.o test/*.s
	@rm -rf *.dSYM

.PHONY: testall
testall:
	@for case in $(TESTCASES); do \
		echo "================================================="; \
		echo $$case; \
		echo "================================================="; \
		$(MAKE) jit_clean; \
		ocamlbuild $(OCAMLBUILD_OPTIONS) test/$$case.byte || exit 1; \
		$(MAKE) jit_clean; \
		./$$case.byte; \
	done

.PHONY: runtest
runtest:
	@ocamlbuild $(OCAMLBUILD_OPTIONS) $(T).byte

.PHONY: gcc
gcc:
	./min-caml test/data/$(INTERP) && \
	gcc -c -m32 $(TRACE).s && \
	gcc -g -Wall -O2 -m32 lib/libmincaml.S lib/stub.c test/data/$(INTERP).s $(TRACE).o -o $(TRACE) && \
	rm -rf $(TRACE).dSYM

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
