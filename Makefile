default: compiler interp

.all: compiler interp clean example test

OCAMLMAKEFILE = Makefile.ocamlmakefile

SUBDIRS = src

TESTCASES = jitTest interpTest pypysampleTest

PACKS = ounit,str

.PHONY: subdirs $(SUBDIRS)

compiler: subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@
	@mv src/min-caml{,.top} .

.PHONY: interp
interp:
	ocamlbuild -Is src,test -pkgs $(PACKS) src/interpMain.byte
	mv interpMain.byte min-camli

clean:
	@for dir in $(SUBDIRS); do \
	  rm -f $$dir/{*.o,*.cmi,*.cmo}; \
	done
	@for case in $(TESTCASES); do \
	  rm -f $$case.byte; \
	done
	@rm -f min-caml{,.top}

.PHONY: test
test:
	@for case in $(TESTCASES); do \
	  ocamlbuild -Is src,test -pkgs $(PACKS) test/$$case.byte; \
	  ./$$case.byte; \
	done
	@rm -f min-camli
