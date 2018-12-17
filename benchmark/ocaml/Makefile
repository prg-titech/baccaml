OCAMLOPT := ocamlfind ocamlopt

LIBS := -linkpkg -package unix

SRC := time.ml

APP := fib

$(APP): $(APP).ml
	$(OCAMLOPT) $(LIBS) -o $(basename $<) $(SRC) $<

clean:
	rm -f $(APP) *.cm* *.o *.s
