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
	util.ml logger.ml float.c type.ml id.ml m.ml s.ml \
	syntax.ml parser.mly lexer.mll typing.mli typing.ml kNormal.mli kNormal.ml \
	alpha.mli alpha.ml beta.mli beta.ml assoc.mli assoc.ml \
	inline.mli inline.ml constFold.mli constFold.ml elim.mli elim.ml \
	closure.mli closure.ml asm.mli asm.ml virtual.mli virtual.ml \
	simm.mli simm.ml regAlloc.mli regAlloc.ml emit.mli emit.ml \
	interp.mli interp.ml emitVirtual.mli emitVirtual.ml \
	main.mli main.ml top.mli top.ml

TESTS = \
	print sum-tail gcd sum fib ack even-odd \
	adder funcomp cls-rec cls-bug cls-bug2 cls-reg-bug \
	shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
	join-reg join-reg2 non-tail-if non-tail-if2 \
	inprod inprod-rec inprod-loop matmul matmul-flat \
	manyargs fib-tail array

INTERP_TESTS = \
	print sum-tail gcd sum even-odd \
	adder cls-rec cls-bug cls-reg-bug \
	shuffle spill spill2 spill3 join-stack join-stack2 join-stack3 \
	manyargs fib-tail

UNPASSED_TESTS = \
	fib ack funcomp cls-bug2 join-reg join-reg2 non-tail-if non-tail-if2 \
	inprod inprod-rec inprod-loop matmul matmul-flat

PACKS = str ounit

test: $(TESTS:%=test/%.cmp)

exec:
	@for target in $(INTERP_TESTS); do\
	  echo "\033[35mTEST $$target\033[0m";\
	  ./min-caml -interp "test/$$target";\
	  echo "";\
	  echo "\033[33mEXECUTED $$target\033[0m\n";\
	done

exec_unpass:
	@for target in $(UNPASSED_TESTS); do\
	  echo "\033[35mTEST $$target\033[0m";\
	  ./min-caml -interp "test/$$target";\
	  echo "";\
	  echo "\033[33mEXECUTED $$target\033[0m\n";\
	done


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
