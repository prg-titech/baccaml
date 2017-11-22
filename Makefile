default: compiler interp

.all: compiler interp clean test

OCAMLMAKEFILE = Makefile.ocamlmakefile

compiler:
	$(MAKE) -f $(OCAMLMAKEFILE)

interp:
	$(MAKE) -f $(OCAMLMAKEFILE) PROJECT=interpreter

clean:
	$(MAKE) -f $(OCAMLMAKEFILE) clean

.PHONY: test
test: compiler
	$(MAKE) -f $(OCAMLMAKEFILE) test

jitcheck:
	ocamlfind ocamlc -package ounit,str -linkpkg -o jitTest util.ml s.ml id.ml type.ml asm.ml jit.ml jitTest.ml
	./jitTest
