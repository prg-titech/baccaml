SUBDIRS := baccaml mincaml c pypy
APP := fib
CALC :=

all: $(SUBDIRS)
$(SUBDIRS):
	$(MAKE) -C $@ APP=$(APP)

calc:
	@printf "Average: "; awk -f average.awk $(CALC)
	@printf "SD: "; awk -f standard_deviation.awk $(CALC)

.PHONY: all $(SUBDIRS)
