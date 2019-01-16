SUBDIRS := baccaml mincaml c pypy
APP := fib
CALC :=

all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ APP=$(APP)

LOG := foo.log
data:
	@echo "Average"
	@awk -f average.awk $(LOG)	
	@echo "SD"
	@awk -f standard_deviation.awk $(LOG)	

.PHONY: all $(SUBDIRS)
