build:
	ocamlc -c interp.ml
	ocamlc -c interpSpec.ml
	ocamlc -o test interp.cmo interpSpec.cmo

clean:
	rm -f *.cmi *.cmo test

test: build
	./test
