# -*- Makefile -*-
#
# build the rkinz program for convenient calculation of expected energy losses
#

DEFINES        = 
INCLUDES       = -I..

ifeq ($(UIO_APPLICATIONS),)
INSTALL_DIR = /Applications/prog
else
INSTALL_DIR = $(UIO_APPLICATIONS)/prog
endif
INSTALL_BIN = install -m 0755
#$(warning install in $(INSTALL_DIR))

LIBS           = $(shell root-config --glibs)

KINZ           = ../kinzlib.o
AME            = ../ame2003_masses.o
RKINZ          = rkinz
TARGETS        = $(RKINZ)

RKINZ_SRCS     = rkinz.cpp
RKINZ_OBJS     = $(RKINZ_SRCS:%.cpp=%.o)

ALL_OBJS = $(sort $(RKINZ_OBJS))

CXXFLAGS       = -Wall -W -g -O2 $(INCLUDES) $(DEFINES) $(shell root-config --cflags)

CXX            = g++
RM             = rm -f
CP             = cp
ECHO           = /bin/echo

# for automatic dependency files
DEPEND = .depend-
DEPFILES = $(ALL_OBJS:%.o=$(DEPEND)%.dep)

all: $(RKINZ)

install: $(RKINZ)
	test -d $(INSTALL_DIR)/bin || mkdir $(INSTALL_DIR)/bin
	$(INSTALL_BIN) rkinz $(INSTALL_DIR)/bin/rkinz

$(RKINZ): $(RKINZ_OBJS) rootdict.o $(KINZ) ../ame2003_masses.o ../ask_par.o
	@echo "   [LINK] $@"
	@$(CXX)  -o $@ $^ $(LIBS)

$(KINZ):
	@$(MAKE) -C $(shell dirname $(KINZ)) $(shell basename $(KINZ))

$(AME):
	@$(MAKE) -C $(shell dirname $(AME)) $(shell basename $(AME))

../ask_par.o: ../ask_par.c
	@$(MAKE) -C .. ask_par.o

%.o: %.cpp $(DEPEND)%.dep
	@echo "    [CXX] $<"
	@$(CXX) $(CXXFLAGS) -o $@ -c $<

rootdict.cpp: rkinz.h linkdef.h
	@echo "   [CINT] $<"
	rootcint -f $@ -c $^

clean:  
	$(RM) *.o a.out core $(UPDATE) rootdict.cpp rootdict.h

very-clean: clean
	rm -f $(DEPFILES)

# automatically create dependency files
#$(SORT_OBJS:%.o=$(DEPEND)%.dep):  $(DEPEND)%.dep: ../sort/%.cpp
#	@echo "    [DEP] $<"
#	@( $(ECHO) -n "$@ "; $(CXX) $(CXXFLAGS) -MM $< ) > $@ || rm -f $@
$(DEPEND)%.dep: %.cpp
	@echo "    [DEP] $<"
	@( $(ECHO) -n "$@ "; $(CXX) $(CXXFLAGS) -MM $< ) > $@ || rm -f $@

-include $(DEPFILES)

.SUFFIXES: # Delete the default suffixes
