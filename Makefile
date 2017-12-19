default: compiler interp

.all: compiler interp clean example test

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31
COMPILER = min-caml
INTERPRETER = min-camli

OCAMLBUILD_OPTIONS = -use-ocamlfind -lflags -custom,src/float.o

TESTCASES = jitTest interpTest pypyfig3Test simple1Test

EXAMPLES = print sum-tail gcd sum fib ack even-odd adder \
funcomp cls-rec cls-bug cls-bug2 cls-reg-bug shuffle \
spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 inprod inprod-rec \
inprod-loop matmul matmul-flat manyargs fib-tail array array2 \
float tuple

SUBDIRS = src

.PHONY: compiler
compiler:
	@ocamlbuild src/float.o &>/dev/null
	@ocamlbuild $(OCAMLBUILD_OPTIONS) src/main.byte
	@mv main.byte min-caml

.PHONY: interp
interp:
	@ocamlbuild $(OCAMLBUILD_OPTIONS) src/interpMain.byte
	@mv interpMain.byte min-camli

.PHONY: clean
clean:
	@rm -f $(TRASH)
	ocamlbuild -clean

.PHONY: test
test:
	@for case in $(TESTCASES); do \
		ocamlbuild src/float.o &>/dev/null; \
		ocamlbuild -Is src,test $(OCAMLBUILD_OPTIONS) test/$$case.byte || exit 1; \
		./$$case.byte; \
	done

.PHONY: pypytest
pypytest:
	ocamlbuild src/float.o &>/dev/null
	ocamlbuild -Is src,test $(OCAMLBUILD_OPTIONS) test/pypyfig3Test.byte
	./pypyfig3Test.byte

.PHONY: simple1test
simple1test:
	ocamlbuild src/float.o &>/dev/null
	ocamlbuild -I src $(OCAMLBUILD_OPTIONS) test/simple1Test.byte
	./simple1Test.byte

.PHONY: example
example: $(EXAMPLES:%=example/%.cmp)

.PRECIOUS: example/%.s example/% example/%.res example/%.ans example/%.cmp
TRASH = $(EXAMPLES:%=example/%.s) $(EXAMPLES:%=example/%) $(EXAMPLES:%=example/%.res) $(EXAMPLES:%=example/%.ans) $(EXAMPLES:%=example/%.cmp)

example/%.s: compiler example/%.ml
	./$(COMPILER) example/$*
example/%: example/%.s src/libmincaml.S src/stub.c
	$(CC) $(CFLAGS) -m32 $^ -lm -o $@
example/%.res: example/%
	$< > $@
example/%.ans: example/%.ml
	ocaml $< > $@
example/%.cmp: example/%.res example/%.ans
	diff $^ > $@
