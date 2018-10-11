SUBDIRS := baccaml mincaml c pypy
APP := fib

all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ APP=$(APP)

.PHONY: all $(SUBDIRS)
