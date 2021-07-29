TOOLS:=$(wildcard */.)

all clean: $(TOOLS)

.PHONY: $(TOOLS)
$(TOOLS):
	$(MAKE) -C $@ $(MAKECMDGOALS)
