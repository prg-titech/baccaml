interp-mj:
	cd ../../ && \
	dune exec benchmark/baccaml/test_fib.exe -- \
		-file benchmark/baccaml/test_interp_mj.ml \
		-green "bytecode 100; pc 0" \
		-red "stack 0; sp 1" \
		-code "8 0 4 2 3 5 11 4 1 14 26 8 0 4 1 1 6 0 8 1 4 2 1 6 0 0 7 4 10 6 0 9"  \
		-o test_fib_mj \
		-type mjit && \
	dune exec src/bin/baccaml_cli.exe -- -type mjit -build -trace test_fib_mj benchmark/baccaml/test_interp_mj.ml; \
	mv test_fib_mj benchmark/baccaml


interp-tj:
	cd ../../ && \
	dune exec benchmark/baccaml/test_fib.exe -- \
		-file benchmark/baccaml/test_interp_tj.ml \
		-green "bytecode 100; pc 0" \
		-red "stack 0; sp 2" \
		-code "8 1 4 2 3 5 11 4 1 14 26 8 1 4 1 1 6 0 8 2 4 2 1 6 0 0 7 1 4 10 6 0 9"  \
		-o test_fib_tj \
		-type tjit && \
	dune exec src/bin/baccaml_cli.exe -- -type tjit -build -trace test_fib_tj benchmark/baccaml/test_interp_tj.ml && \
	mv test_fib_tj benchmark/baccaml

interp-naive:
	cd ../../ && \
	dune exec src/base/min_caml.exe -- -type mjit benchmark/baccaml/test_interp_naive.ml; \
	gcc -m32 stub/stub.c stub/libmincaml.S benchmark/baccaml/test_interp_naive.s -o benchmark/baccaml/test_interp_naive
