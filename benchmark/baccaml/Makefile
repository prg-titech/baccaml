APPS = test_fib_mj test_fib_tj test_interp_naive test_interp_sum test_sum_mj test_sum_tj
APP := fib
JIT := mj

CLI := src/bin/baccaml_cli.exe
BUILDER := src/bin/baccaml_builder.exe

ifeq ($(APP), fib)
$(APP): test_fib_tj test_fib_mj test_interp_naive
else
ifeq ($(APP), sum)
$(APP): test_sum_tj test_sum_tj test_interp_sum
endif
endif

all: $(APPS)

test_fib_tj:
	cd ../../ && \
	dune exec $(CLI) -- \
		-file benchmark/baccaml/test_interp_tj.ml \
		-green "bytecode 100; pc 0" \
		-red "stack 0; sp 3" \
		-code "8 1 4 2 3 5 11 4 1 14 26 8 1 4 1 1 6 0 8 2 4 2 1 6 0 0 7 1 4 10 6 0 9"  \
		-o test_fib_tj \
		-type tjit && \
	dune exec $(BUILDER) -- -type tjit -build -trace test_fib_tj benchmark/baccaml/test_interp_tj.ml && \
	mv test_fib_tj benchmark/baccaml

test_fib_mj:
	cd ../../ && \
	dune exec $(CLI) -- \
		-file benchmark/baccaml/test_interp_mj.ml \
		-green "bytecode 100; pc 0" \
		-red "stack 0; sp 1" \
		-code "8 0 4 2 3 5 11 4 1 14 26 8 0 4 1 1 6 0 8 1 4 2 1 6 0 0 7 4 10 6 0 9"  \
		-o test_fib_mj \
		-type mjit && \
	dune exec $(BUILDER) -- -type mjit -build -trace test_fib_mj benchmark/baccaml/test_interp_mj.ml; \
	mv test_fib_mj benchmark/baccaml

test_sum_mj: test_sum.ml
	$(eval INTERP := test_interp_sum_mj)
	cd ../../ && \
	dune exec $(CLI) -- \
		-file benchmark/baccaml/$(INTERP).ml \
		-green "bytecode 100; pc 0" \
		-red "stack 0; sp 1" \
		-code "8 0 4 2 3 5 11 4 1 14 21 8 0 8 1 4 1 1 6 0 0 7 4 10 6 0 9" \
		-o $(basename $<)_mj \
		-type mjit && \
	dune exec $(BUILDER) -- -type mjit -build -trace test_sum_mj  \
		benchmark/baccaml/$(INTERP).ml; \
	mv $(basename $<)_mj benchmark/baccaml

test_sum_tj: test_sum.ml
	$(eval INTERP := test_interp_sum_tj)
	cd ../../ && \
	dune exec $(CLI) -- \
		-file benchmark/baccaml/$(INTERP).ml \
		-green "bytecode 100; pc 0" \
		-red "stack 0; sp 2" \
		-code "8 1 4 2 3 5 11 4 1 14 21 8 1 8 2 4 1 1 6 0 0 7 1 4 10 6 0 9" \
		-o $(basename $<)_tj \
		-type tjit && \
	dune exec $(BUILDER) -- -type tjit -build -trace test_sum_tj \
		benchmark/baccaml/$(INTERP).ml; \
	mv $(basename $<)_tj benchmark/baccaml

test_interp_naive: test_interp_naive.ml
	cd ../../ && \
	dune exec src/base/min_caml.exe -- -type mjit benchmark/baccaml/test_interp_naive.ml; \
	gcc -m32 stub/stub.c stub/libmincaml.S benchmark/baccaml/test_interp_naive.s -o benchmark/baccaml/test_interp_naive

test_interp_sum: test_interp_sum.ml
	cd ../../ && \
	dune exec src/base/min_caml.exe -- -type mjit benchmark/baccaml/$<; \
	gcc -m32 stub/stub.c stub/libmincaml.S benchmark/baccaml/$(basename $<).s -o benchmark/baccaml/$(basename $<)

clean:
	$(RM) $(APPS)
