default: compiler interp

.all: compiler interp clean example test

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31
COMPILER = min-caml
INTERPRETER = min-camli

OCAMLBUILD_OPTIONS = -use-ocamlfind

TESTCASES = jitTest methodJitTest interpTest pypyfig3Test simple1Test

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
	@ocamlbuild $(OCAMLBUILD_OPTIONS) src/jit/interpMain.byte
	@mv interpMain.byte min-camli

.PHONY: clean
clean:
	@rm -f $(TRASH)
	ocamlbuild -clean

.PHONY: test
test:
	@for case in $(TESTCASES); do \
		echo ;\
		echo $$case; \
		echo ;\
		ocamlbuild -Is src,test $(OCAMLBUILD_OPTIONS) $$case.byte || exit 1; \
		./$$case.byte; \
	done

.PHONY: pypytest
pypytest: test/pypyfig3Test.ml
	ocamlbuild -Is src,test $(OCAMLBUILD_OPTIONS) $(basename $<).byte
	./pypyfig3Test.byte

.PHONY: simple1test
simple1test: test/simple1Test.ml
	ocamlbuild $(OCAMLBUILD_OPTIONS) $(basename $<).byte
	./simple1Test.byte -debug -jit

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
