default: compiler interp

.all: compiler interp clean test

OCAMLMAKEFILE = Makefile.ocamlmakefile

JITFILES= util.ml s.ml id.ml type.ml jit.ml jitTest.ml

JITTESTPROGRAM = jitTest

compiler:
	$(MAKE) -f $(OCAMLMAKEFILE)

interp:
	$(MAKE) -f $(OCAMLMAKEFILE) PROJECT=interpreter

clean:
	$(MAKE) -f $(OCAMLMAKEFILE) clean

.PHONY: test
test: compiler
	$(MAKE) -f $(OCAMLMAKEFILE) test

.PHONY: jitcheck
jitcheck:
	ocamlfind ocamlc -package ounit,str -linkpkg -o $(JITTESTPROGRAM) $(JITFILES)
	./$(JITTESTPROGRAM) && rm -f $(JITTESTPROGRAM)
