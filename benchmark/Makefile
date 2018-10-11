SUBDIRS := $(wildcard */.)
APP := fib

all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ APP=$(APP)

.PHONY: all $(SUBDIRS)
