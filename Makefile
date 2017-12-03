default: compiler interp

.all: compiler interp clean test

OCAMLMAKEFILE = Makefile.ocamlmakefile

JITFILES= util.ml type.ml id.ml m.ml s.ml asm.mli asm.ml jitUtil.ml jit.ml jitTest.ml

INTERPFILES = util.ml logger.ml type.ml id.ml m.ml s.ml asm.mli asm.ml jitUtil.ml interp.mli interp.ml interpTest.ml

JITTEST = jitTest

INTERPTEST = interpTest

compiler:
	$(MAKE) -f $(OCAMLMAKEFILE)

interp:
	$(MAKE) -f $(OCAMLMAKEFILE) PROJECT=interpreter

clean:
	$(MAKE) -f $(OCAMLMAKEFILE) clean
	rm -f $(JITTESTPROGRAM) $(INTERPTEST)

.PHONY: test
test: compiler
	$(MAKE) -f $(OCAMLMAKEFILE) test

.PHONY: jitcheck
jitcheck:
	ocamlfind ocamlc -package ounit,str -linkpkg -o $(JITTEST) $(JITFILES)
	./$(JITTEST); rm -f $(JITTEST)

.PHONY: interpcheck
interpcheck:
	ocamlfind ocamlc -package ounit,str -linkpkg -o $(INTERPTEST) $(INTERPFILES)
	./$(INTERPTEST); rm -f $(INTERPTEST)

.PHONY: allcheck
allckeck: jitcheck interpcheck
