# SUBDIRS := $(wildcard */.)
SUBDIRS := c mincaml

all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@

.PHONY: all $(SUBDIRS)
