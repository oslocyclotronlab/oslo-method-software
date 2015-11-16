
CC       = gcc
CFLAGS   = -O2 -g -Wall -W
CXX      = g++
CXXFLAGS = -O2 -g -Wall -W
LIBS     = -lz -lbz2 -lm
LD            = $(CXX)
LDLIB         = $(CXX) -shared

TARGETS = kinz ame2003

DEPEND = .depend-

CXX_SOURCES = ame2003.cpp ame2003_masses.cpp kinz.cpp
CXX_DEPEND = $(CXX_SOURCES:%.cpp=$(DEPEND)%.dep)
CXX_OBJECTS = $(CXX_SOURCES:%.cpp=%.o)

C_SOURCES = ask_par.c
C_DEPEND  = $(C_SOURCES:%.c=$(DEPEND)%.dep)
C_OBJECTS = $(C_SOURCES:%.c=%.o)

DEPFILES = $(C_DEPEND) $(CXX_DEPEND)

all: $(TARGETS)

kinz: kinz.o ame2003_masses.o ask_par.o
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
