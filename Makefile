# Sumii's Makefile for Min-Caml (for GNU Make)

RESULT = min-caml
NCSUFFIX = .opt
CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31

default: debug-code top $(RESULT) # do_test
$(RESULT): debug-code top

clean:: nobackup

SOURCES = \
	float.c type.ml id.ml m.ml s.ml \
	syntax.ml parser.mly lexer.mll typing.mli typing.ml kNormal.mli kNormal.ml \
	alpha.mli alpha.ml beta.mli beta.ml assoc.mli assoc.ml \
	inline.mli inline.ml constFold.mli constFold.ml elim.mli elim.ml \
	closure.mli closure.ml asm.mli asm.ml virtual.mli virtual.ml \
	simm.mli simm.ml regAlloc.mli regAlloc.ml emit.mli emit.ml \
	emitVirtual.mli emitVirtual.ml logger.ml util.ml interp.mli interp.ml \
	main.mli main.ml top.mli top.ml

TESTS = \
	print sum-tail gcd sum fib ack even-odd \
	adder funcomp cls-rec cls-bug cls-bug2 cls-reg-bug \
	shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
	join-reg join-reg2 non-tail-if non-tail-if2 \
	inprod inprod-rec inprod-loop matmul matmul-flat \
	manyargs fib-tail

PACKS = str ounit

test: $(TESTS:%=test/%.cmp)

to_ppc:
	ln -sf PowerPC/{*.ml{,i},libmincaml.S} .
	cd test; ln -sf PowerPC/{,too}manyargs.ml .; cd ..
	cd shootout; ln -sf PowerPC/Makefile .; cd ..
	cd bytemark; ln -sf PowerPC/Makefile .; cd ..
	cd min-rt; ln -sf PowerPC/globals.s .; cd ..

to_x86:
	ln -f x86/{*.ml{,i},libmincaml.S} .
	cd test || exit; ln -sf x86/{,too}manyargs.ml .; cd .. || exit
	cd shootout || exit; ln -f x86/Makefile .; cd .. || exit
	cd bytemark || exit; ln -sf x86/Makefile .; cd .. || exit
	cd min-rt || exit ; ln -f x86/globals.s .; cd .. || exit

to_sparc:
	ln -sf SPARC/{*.ml{,i},libmincaml.S} .
	cd test; ln -sf SPARC/{,too}manyargs.ml .; cd ..
	cd shootout; ln -sf SPARC/Makefile .; cd ..
	cd bytemark; ln -sf SPARC/Makefile .; cd ..
	cd min-rt; ln -sf SPARC/globals.s .; cd ..


.PRECIOUS: test/%.s test/% test/%.res test/%.ans test/%.cmp
TRASH = $(TESTS:%=test/%.s) $(TESTS:%=test/%) $(TESTS:%=test/%.res) $(TESTS:%=test/%.ans) $(TESTS:%=test/%.cmp)

test/%.s: $(RESULT) test/%.ml
	./$(RESULT) test/$*
test/%: test/%.s libmincaml.S stub.c
	$(CC) $(CFLAGS) -m32 $^ -lm -o $@
test/%.res: test/%
	$< > $@
test/%.ans: test/%.ml
	ocaml $< > $@
test/%.cmp: test/%.res test/%.ans
	diff $^ > $@

-include OCamlMakefile
