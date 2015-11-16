#************************************************************
# file: Makefile
# May 1996 Magne Guttormsen
#
# Aquisition programs for DAISY 
#************************************************************
#
# Rules
#
.c:
	$(CC) -o $* $*.c $(CAMLIB) $(VMELIB) -L/lib/ces -lvme

#
# Configuration
#
CC	= gcc
TARGET	= campari eventbuilder slottest rocket hex peekdaisy vpeek16 peekcamac snutt
VMELIB	= /usr/src/ces/examples/vlib/vlib.o
CAMLIB  = /home/magne/examples/cbd/cbdclib.o

all:	$(TARGET) 

clean:
	rm -f *.o

clobber: clean
	rm -f $(TARGET) *.o
#
# Dependencies
#
campari:      campari.c 
eventbuilder: eventbuilder.c 
slottest:     slottest.c
rocket:       rocket.c
hex:          hex.c
peekdaisy:    peekdaisy.c
vpeek16:      vpeek16.c
peekcamac:    peekcamac.c
snutt:       snutt.c
