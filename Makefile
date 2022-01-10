MAKE = make

all: interpreter compiler

interpreter:
	$(MAKE) -C tac-interpreter

compiler:
	$(MAKE) -C tac2mips

.PHONY: all