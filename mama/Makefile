
all: help
	make -C src $@
	make -C spy $@
	make -C update $@

help:
	@echo
	@echo "============================================================"
	@echo "Help for compiling 'mama':"
	@echo "make all          - to compile"
	@echo "make install      - to compile and install"
	@echo "make clean        - to remove compiler output except programs"
	@echo "make very-clean   - to remove all compiler output"
	@echo "============================================================"
	@echo

%:
	make -C src $@
	make -C spy $@
	make -C update $@
