default: compiler interp

.all: compiler interp clean example test

OCAMLMAKEFILE = Makefile.ocamlmakefile

JITFILES= util.ml type.ml id.ml m.ml s.ml asm.mli asm.ml jitUtil.ml jit.ml jitTest.ml
INTERPFILES = util.ml logger.ml type.ml id.ml m.ml s.ml asm.mli asm.ml jitUtil.ml interp.mli interp.ml interpTest.ml

JITTEST = jitTest
INTERPTEST = interpTest
PYPYSAMPLETEST = pypysampleTest


compiler:
	$(MAKE) -f $(OCAMLMAKEFILE) PROJECT=compiler

interp:
	$(MAKE) -f $(OCAMLMAKEFILE) PROJECT=interpreter

clean:
	$(MAKE) -f $(OCAMLMAKEFILE) clean
	rm -f $(JITTEST) $(INTERPTEST) $(PYPYSAMPLETEST) $(PYPYSAMPLETEST).top

.PHONY: example
example: compiler
	$(MAKE) -f $(OCAMLMAKEFILE) RESULT=min-caml example

.PHONY: jittest
jittest:
	ocamlfind ocamlc -package ounit,str -linkpkg -o $(JITTEST) $(JITFILES)
	./$(JITTEST)

.PHONY: intertest
interptest:
	ocamlfind ocamlc -package ounit,str -linkpkg -o $(INTERPTEST) $(INTERPFILES)
	./$(INTERPTEST)

.PHONY: pypysampletest
pypysampletest:
	$(MAKE) -f $(OCAMLMAKEFILE) PROJECT=trace PROGRAM=$(PYPYSAMPLETEST)
	./$(PYPYSAMPLETEST)

test: jittest interptest pypysampletest
