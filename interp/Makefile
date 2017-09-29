test:
	ocamlc -c interp.ml
	ocamlc -c interpSpec.ml
	ocamlc -o run interp.cmo interpSpec.cmo

clean:
	rm -f *.cmi *.cmo run
