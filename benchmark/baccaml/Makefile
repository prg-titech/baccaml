APPS = test_fib_mj test_fib_tj test_interp_naive test_interp_sum test_sum_mj test_sum_tj
APP := fib
JIT := mj

BACCAML := dune exec baccaml
BUILDER := dune exec baccaml_builder
MINCAML := dune exec min-caml

ifeq ($(APP), fib)
$(APP): test_fib_tj test_fib_mj test_interp_naive
else
ifeq ($(APP), sum)
$(APP): test_sum_tj test_sum_mj test_interp_sum
endif
endif

all: $(APPS)

test_fib_tj:
	cd ../../ && \
	$(BACCAML) -- -file benchmark/baccaml/test_interp_tj.ml benchmark/armin/fib.am benchmark/armin/fib_26.am && \
	$(BUILDER) -- benchmark/baccaml/test_interp_tj.ml -type tjit -trace fib -trace fib_26 -o test_fib_tj -build && \
	mv test_fib_tj benchmark/baccaml

test_fib_mj:
	cd ../../ && \
	$(BACCAML) -- -file benchmark/baccaml/test_interp_mj.ml benchmark/armin/fib_mj.am -elim 1 && \
	$(BUILDER) -- benchmark/baccaml/test_interp_mj.ml -type mjit -trace fib_mj -o test_fib_mj -build && \
	cp test_fib_mj benchmark/baccaml

test_sum_mj:
	cd ../../ && \
	$(BACCAML) -- -file benchmark/baccaml/test_interp_sum_mj.ml benchmark/armin/sum_mj.am && \
	$(BUILDER) -- benchmark/baccaml/test_interp_sum_mj.ml -type mjit -trace sum_mj -o test_sum_mj -build && \
	mv test_sum_mj benchmark/baccaml

test_sum_tj:
	cd ../../ && \
	$(BACCAML) -- -file benchmark/baccaml/test_interp_sum_tj.ml benchmark/armin/sum.am benchmark/armin/sum_20.am && \
	$(BUILDER) -- benchmark/baccaml/test_interp_sum_tj.ml -type tjit -trace sum -trace sum_20 -o test_sum_tj -build && \
	mv test_sum_tj benchmark/baccaml

test_interp_naive: test_interp_naive.ml
	cd ../../ && \
	$(MINCAML) -- -type mjit benchmark/baccaml/test_interp_naive.ml; \
	gcc -m32 stub/stub.c stub/libmincaml.S benchmark/baccaml/test_interp_naive.s -o benchmark/baccaml/test_interp_naive

test_interp_sum: test_interp_sum.ml
	cd ../../ && \
	$(MINCAML) -- -type mjit benchmark/baccaml/$<; \
	gcc -m32 stub/stub.c stub/libmincaml.S benchmark/baccaml/$(basename $<).s -o benchmark/baccaml/$(basename $<)

clean:
	$(RM) $(APPS)

clean-dSYM:
	cd ../../ && \
	$(RM) -rf *.dSYM
