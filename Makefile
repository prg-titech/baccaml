default: compiler interp

.all: compiler interp clean test

OCAMLMAKEFILE = Makefile.ocamlmakefile

JITFILES= util.ml type.ml id.ml m.ml s.ml asm.mli asm.ml jit.ml jitTest.ml

JITTESTPROGRAM = jitTest

compiler:
	$(MAKE) -f $(OCAMLMAKEFILE)

interp:
	$(MAKE) -f $(OCAMLMAKEFILE) PROJECT=interpreter

clean:
	$(MAKE) -f $(OCAMLMAKEFILE) clean
	rm -f $(JITTESTPROGRAM)

.PHONY: test
test: compiler
	$(MAKE) -f $(OCAMLMAKEFILE) test

.PHONY: jitcheck
jitcheck:
	ocamlfind ocamlc -package ounit,str -linkpkg -o $(JITTESTPROGRAM) $(JITFILES)
	./$(JITTESTPROGRAM)
