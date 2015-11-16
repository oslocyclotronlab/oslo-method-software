
CC       = gcc
CFLAGS   = -O2 -g -Wall -W
CXX      = g++
CXXFLAGS = -O2 -g -Wall -W
LIBS     = -lz -lbz2 -lm
LD            = $(CXX)
LDLIB         = $(CXX) -shared

ifeq ($(UIO_APPLICATIONS),)
INSTALL_DIR = /Applications/prog
else
INSTALL_DIR = $(UIO_APPLICATIONS)/prog
endif
INSTALL_BIN = install -m 0755
#$(warning install in $(INSTALL_DIR))

TARGETS = kinz ame2003 zrange

DEPEND = .depend-

CXX_SOURCES = ame2003.cpp ame2003_masses.cpp kinz.cpp zrange.cpp kinzlib.cpp
CXX_DEPEND = $(CXX_SOURCES:%.cpp=$(DEPEND)%.dep)
CXX_OBJECTS = $(CXX_SOURCES:%.cpp=%.o)

C_SOURCES = ask_par.c
C_DEPEND  = $(C_SOURCES:%.c=$(DEPEND)%.dep)
C_OBJECTS = $(C_SOURCES:%.c=%.o)

DEPFILES = $(C_DEPEND) $(CXX_DEPEND)

all: $(TARGETS)
	$(MAKE) -C rkinz all

install: $(TARGETS)
	test -d $(INSTALL_DIR)/bin || mkdir $(INSTALL_DIR)/bin
	for program in $(TARGETS) ; \
	    do $(INSTALL_BIN) $$program $(INSTALL_DIR)/bin/$$program; \
	done
	$(MAKE) -C rkinz install

kinz: kinz.o ame2003_masses.o ask_par.o kinzlib.o
	$(LD) $(LIBS) -o $@ $^

zrange: zrange.o
	$(LD) $(LIBS) -o $@ $^

ame2003: ame2003.o ame2003_masses.o ask_par.o
	$(LD) $(LIBS) -o $@ $^

ame2003_masses.cpp: ame2003_mass_tables.cppi
ame2003_mass_tables.cppi: ame2003mass2cpp.py AMEmass.mas03
	python ame2003mass2cpp.py AMEmass.mas03 > $@ || rm -f $@

####################

$(CXX_OBJECTS): %.o: %.cpp $(DEPEND)%.dep
	$(CXX) $(CXXFLAGS) -c -o $@ $(@:%.o=%.cpp)

$(C_OBJECTS): %.o: %.c $(DEPEND)%.dep
	$(CC) $(CFLAGS) -c -o $@ $(@:%.o=%.c)

clean:
	rm -f $(CXX_OBJECTS) $(C_OBJECTS) ame2003_mass_tables.cppi

very-clean: clean
	rm -f $(TARGETS) $(DEPFILES)

# automatically create dependency files
$(C_DEPEND): $(DEPEND)%.dep: %.c
	$(CC) $(CFLAGS) -MM $< > $@

$(CXX_DEPEND): $(DEPEND)%.dep: %.cpp
	$(CXX) $(CXXFLAGS) -MM $< > $@

-include $(DEPFILES)

.PHONY: all

.SUFFIXES:
