# SUBDIRS := $(wildcard */.)
SUBDIRS := c pypy

all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: all $(SUBDIRS)
